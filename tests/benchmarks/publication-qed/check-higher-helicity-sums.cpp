// Compile with C++17, -O3, -I csource and the HEPCAT native library.
// Run with CONTRACTED_TABLE_DIRECTORY PRINTED_ONE_MINUS_DIRECTORY POINTS FIRST_SEED.
// The equal-helicity formulas are transcribed from the numbered summands.
// The mixed-helicity polynomials are read back from the LaTeX manuscript.
#include "contracted-evaluation.hpp"
#include <filesystem>

using namespace contracted_qed;

mask leg(unsigned i) { return 1u << (i - 1); }

complex printed_sum(const point& p, real e, real m) {
    const auto n = p.masses.size();
    // No polarization substitution here. Each bracket is an ordinary external
    // spinor and each insertion is an explicit sum of external momenta.
    const kinematics k(p, std::vector<int>(n, 1), true, -1);
    auto tau = [&](mask momentum) { return k.inv[momentum] - m*m; };
    auto square = [&](unsigned a, unsigned b) {
        return k.evaluate({a-1, b-1, false, false, {}});
    };
    auto angle = [&](unsigned a, unsigned b) {
        return k.evaluate({a-1, b-1, true, true, {}});
    };
    std::vector<unsigned> photons(n-2);
    std::iota(photons.begin(), photons.end(), 3u);
    complex sum{};
    do {
        if (n == 8) {
            const auto a=photons[0], b=photons[1], c=photons[2], d=photons[3];
            const auto f=photons[4], g=photons[5];
            auto x = [&](unsigned i) {
                return k.evaluate({i-1, i-1, false, false, {leg(1),leg(2)}}) / tau(leg(2)|leg(i));
            };
            const auto first_pair = tau(leg(2)|leg(a)|leg(b));
            const auto second_pair = tau(leg(1)|leg(c)|leg(d));
            const auto u = square(c,f)*k.evaluate({f-1,c-1,false,true,{leg(1)}});
            const auto v = k.evaluate({g-1,g-1,false,false,{leg(1),leg(c)|leg(d)|leg(f)}});
            const auto correction = -x(f)*x(g) + m*m*std::pow(square(f,g),2)/first_pair
                + 4.L*u/tau(leg(1)|leg(c)|leg(d)|leg(f))*(x(g)-v/first_pair);
            const auto numerator = x(c)*x(d)*x(f)*x(g)/(tau(leg(1)|leg(a))*tau(leg(1)|leg(b)))
                + 6.L*m*m*std::pow(square(c,d),2)*correction/(tau(leg(2)|leg(a))*tau(leg(2)|leg(b))*second_pair);
            sum += std::pow(square(a,b),2)*numerator
                /(tau(leg(1)|leg(c))*tau(leg(1)|leg(d))*tau(leg(1)|leg(f))*tau(leg(1)|leg(g)));
        } else {
            const auto a=photons[5], b=photons[6];
            complex term=square(a,b)/(angle(a,b)*tau(leg(2)|leg(a)));
            mask previous=leg(1);
            for (unsigned j=0; j<5; ++j) {
                const auto i=photons[j];
                const auto sandwich=k.evaluate({a-1,i-1,true,false,{previous}});
                previous |= leg(i);
                term *= sandwich/(angle(a,i)*tau(previous));
            }
            sum += term;
        }
    } while (std::next_permutation(photons.begin(),photons.end()));
    const auto scalar = n==8 ? std::pow(e,6)/6.L : -std::pow(std::sqrt(2.L)*e,7);
    return scalar*m*angle(1,2)*sum;
}

complex seven_photon_physical(const point& p, real e, real m) {
    const kinematics k(p,std::vector<int>(9,1),true,-1);
    auto tau=[&](mask g) {return k.inv[g]-m*m;};
    auto sq=[&](unsigned i,unsigned j) {return k.evaluate({i-1,j-1,false,false,{}});};
    auto chain=[&](unsigned i,mask left,mask right) {return k.evaluate({i-1,i-1,false,false,{left,right}});};
    auto x=[&](unsigned i) {return chain(i,leg(1),leg(2))/tau(leg(2)|leg(i));};
    std::vector<unsigned> photons{3,4,5,6,7,8,9};
    real den=1;
    for (auto j:photons) den*=tau(leg(1)|leg(j));
    complex sum{};
    do {
        const auto a=photons[0],b=photons[1],c=photons[2],d=photons[3],f=photons[4],g=photons[5],h=photons[6];
        const mask ab=leg(a)|leg(b), cd=leg(c)|leg(d);
        const auto core=tau(leg(2)|cd), abf=tau(leg(1)|ab|leg(f));
        const auto f_chain=chain(f,ab,leg(1)), g_chain=chain(g,ab|leg(f),leg(1));
        const auto h_chain=chain(h,ab|leg(f)|leg(g),leg(1));
        const auto correction=x(f)*x(g)*x(h)
            +3.L*f_chain/abf*(x(g)*x(h)-m*m*std::pow(sq(g,h),2)/core)
            +3.L/tau(leg(2)|cd|leg(h))*(2.L*f_chain*g_chain/abf-m*m*std::pow(sq(f,g),2))*(x(h)+h_chain/core);
        sum+=std::pow(sq(a,b),2)*(x(c)*x(d)*x(f)*x(g)*x(h)-
            10.L*m*m*std::pow(sq(c,d),2)*tau(leg(1)|leg(c))*tau(leg(1)|leg(d))*correction/
            (tau(leg(2)|leg(c))*tau(leg(2)|leg(d))*tau(leg(1)|ab)));
    } while (std::next_permutation(photons.begin(),photons.end()));
    return m*std::pow(std::sqrt(2.L)*e,7)*k.evaluate({0,1,true,true,{}})*sum/(240.L*den);
}

struct one_minus_sum {
    // The input is the signed polynomial read from the actual LaTeX display.
    // Cache ordinary external spinor chains across the whole permutation sum.
    struct image {
        int sign;
        std::vector<mask> poles;
        std::vector<std::pair<int,std::vector<unsigned>>> terms;
    };
    unsigned legs;
    std::vector<spin_word> words;
    std::vector<image> images;
    one_minus_sum(const std::filesystem::path& file, unsigned n) : legs(n) {
        std::ifstream in(file);
        std::string line, token;
        if (!std::getline(in,line)) throw std::runtime_error("Missing printed sum: "+file.string());
        std::istringstream header(line);
        int exchange;
        header>>exchange;
        std::vector<mask> poles;
        while(header>>token) poles.push_back(labels(token.substr(1)));
        std::vector<std::pair<int,std::vector<std::string>>> terms;
        while(std::getline(in,line)) {
            std::istringstream row(line);
            auto &term=terms.emplace_back();
            row>>term.first;
            while(row>>token) term.second.push_back(token);
        }
        std::map<spin_word,unsigned> ids;
        perm order(n);
        std::iota(order.begin(),order.end(),0);
        do {
            for (int swapped=0;swapped<2;++swapped) {
                if (swapped) std::swap(order[0],order[1]);
                auto &im=images.emplace_back();
                im.sign=swapped?exchange:1;
                for (auto g:poles) im.poles.push_back(remap(g,order));
                for (const auto &[c,product]:terms) {
                    auto &out=im.terms.emplace_back();
                    out.first=c;
                    for (const auto &factor:product) {
                        if (factor=="m") {out.second.push_back(0); continue;}
                        auto w=parse_word(factor);
                        auto endpoint=[&](unsigned i,bool angle) {
                            if (i<2) return order[i];
                            if (angle) return n-1; // all photon angles are <n|
                            return order[i]==n-1?2u:order[i]; // [3| at photon n
                        };
                        w.a=endpoint(w.a,w.angle); w.b=endpoint(w.b,w.end_angle);
                        for (auto &g:w.inserts) g=remap(g,order);
                        auto [it,added]=ids.emplace(w,words.size()+1);
                        if(added) words.push_back(w);
                        out.second.push_back(it->second);
                    }
                }
                if (swapped) std::swap(order[0],order[1]);
            }
        } while(std::next_permutation(order.begin()+2,order.end()));
    }
    complex operator()(const point& p,real e,real m) const {
        const kinematics k(p,std::vector<int>(legs,1),true,-1);
        std::vector<complex> values{m};
        for (const auto &w:words) values.push_back(k.evaluate(w));
        complex norm=k.evaluate({2,legs-1,false,false,{}});
        for (unsigned i=2;i<legs-1;++i) norm*=k.evaluate({legs-1,i,true,true,{}});
        complex sum{},lost{};
        for (const auto &im:images) {
            real den=1;
            for (auto g:im.poles) den*=k.inv[g]-m*m;
            complex num{};
            for (const auto &[c,row]:im.terms) {
                complex term=c;
                for (auto v:row) term*=values[v];
                num+=term;
            }
            auto adjusted=real(im.sign)*num/den-lost;
            auto next=sum+adjusted;
            lost=(next-sum)-adjusted;
            sum=next;
        }
        return std::pow(std::sqrt(2.L)*e,legs-2)*sum/(2.L*norm);
    }
};

int main(int argc, char** argv) try {
    if(argc!=5) throw std::invalid_argument("Use TABLE_DIRECTORY PRINTED_ONE_MINUS_DIRECTORY POINTS FIRST_SEED");
    const auto points=std::stoi(argv[3]);
    if(points<1) throw std::invalid_argument("Positive point count required");
    const auto seed=std::stoull(argv[4]);
    for (unsigned n : {7u,8u,9u}) {
        const auto path=std::filesystem::path(argv[1])/(std::to_string(n)+"-1.spinors");
        const program constructive{table(path.string())};
        const real m=1.L, e=1.L/3;
        std::vector<real> masses(n,0); masses[0]=masses[1]=m;
        std::vector<int> roles(n,2); roles[0]=1; roles[1]=-1;
        const one_minus_sum mixed(std::filesystem::path(argv[2])/(std::to_string(n)+"-minus.spinors"),n);
        real largest=0, mixed_largest=0, physical_largest=0;
        for(int j=0;j<points;++j) {
            const auto p=sample_point(masses,seed+j);
            roles.back()=-2;
            const auto mixed_value=mixed(p,e,m), mixed_ref=constructive(p,roles,true,e,m);
            const auto mixed_error=std::abs(mixed_value-mixed_ref)/std::max(std::abs(mixed_value),std::abs(mixed_ref));
            if(!std::isfinite(mixed_error) || mixed_error>1e-7L) {
                std::cerr<<"One-minus mismatch at "<<n<<" legs, seed "<<seed+j<<": "<<mixed_error<<"\n";
                return 1;
            }
            mixed_largest=std::max(mixed_largest,mixed_error);
            if(n==7) continue;
            roles.back()=2;
            const auto actual=printed_sum(p,e,m), expected=constructive(p,roles,n==8,e,m);
            const auto scale=std::max(std::abs(actual),std::abs(expected));
            const auto err=std::abs(actual-expected)/scale;
            if(!std::isfinite(err) || err>1e-7L) {
                std::cerr<<"Mismatch at "<<n<<" legs, seed "<<seed+j<<": "<<actual<<" vs "<<expected<<"\n";
                return 1;
            }
            largest=std::max(largest,err);
            if(n==9) {
                const auto value=seven_photon_physical(p,e,m);
                const auto delta=std::abs(value-expected)/std::max(std::abs(value),std::abs(expected));
                if(!std::isfinite(delta) || delta>1e-7L) {
                    std::cerr<<"Seven-photon physical mismatch, seed "<<seed+j<<": "<<delta<<"\n";
                    return 1;
                }
                physical_largest=std::max(physical_largest,delta);
            }
        }
        std::cout<<n<<" legs, "<<points<<" one-minus comparisons, maximum relative difference "<<mixed_largest<<std::endl;
        if(n==9) std::cout<<n<<" legs, "<<points<<" physical equal-helicity comparisons, maximum relative difference "<<physical_largest<<std::endl;
        if(n>7) std::cout<<n<<" legs, "<<points<<" equal-helicity comparisons, maximum relative difference "<<largest<<std::endl;
    }
} catch(const std::exception &err) { std::cerr<<err.what()<<"\n"; return 2; }
