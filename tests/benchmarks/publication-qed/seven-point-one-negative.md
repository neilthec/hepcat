# Four positive photons and one negative photon

The fermion and antifermion are legs 1 and 2, both of mass $m$. Write $h=7$ for the negative photon. In both formulas $(a,b,c,d)$ runs over the 24 permutations of $(3,4,5,6)$. Each formula gives the signed amplitude for arbitrary massive external spin indices. All momenta are incoming. We use $\tau_{1ab}=(p_1+p_a+p_b)^2-m^2$, with every leg in the subscript included, and the same convention at end 2.

Define $U=\langle\mathbf1\mathbf2\rangle$, $S=\langle\mathbf1 h\rangle\langle h\mathbf2\rangle$ and $g=(\sqrt{2}e)^5/m$. The orientation of $S$ matters. For either pole choice,
$$
\mathcal M=g\sum_{(a,b,c,d)} K(a,b,c,d).
$$

## AllPhysical

The following three products collect the contributions according to which short fermion currents occur on the two sides of the middle of the line:
$$
\begin{aligned}
4K={}&L_{ab}R_{cd}\big[S-2U V_h(p_c)+\tau_{1ab} B_L\big]\\
&+2D_1(a)\big[V_b(p_a+p_h)R_{cd}+C_{bc}R_d\big]\\
&+L_{ab}\big[4 V_c(p_a)D_2(d)+2 C_{hc}U R_d+C_{cd}B_R\big].
\end{aligned}
$$

Here $V_i$ is linear in its four-vector argument. For a positive photon $i$ and a massive endpoint $t=1,2$, define
$$
 V_i(P)=\frac{[i|p_1 P|i]}{\tau_{1i}},\qquad
 V_h(P)=\frac{\langle h|p_2 P|h\rangle}{\tau_{2h}},\qquad
 b_{ti}=[i|p_t|h\rangle .
$$
In a two-momentum chain the four-vectors denote bispinor insertions. The products joining two adjacent emissions are
$$
 C_{ij}=-\frac{m^2[ij]^2}{\tau_{1i}\tau_{1j}},\qquad
 C_{hi}=-\frac{b_{1i}b_{2i}}{\tau_{2h}\tau_{1i}} .
$$
The positive-photon currents needed here have at most two labels:
$$
 L_{ab}=\frac{C_{ab}}{\tau_{1ab}},\qquad
 R_d=-\frac{V_d(p_2)}{\tau_{2d}},\qquad
 R_{cd}=\frac{V_c(p_2)V_d(p_2)-m^2[cd]^2/\tau_{2cd}}{\tau_{2c}\tau_{2d}}.
$$
The corresponding spin-contracted currents containing $h$ are
$$
 B_L=\frac{S+UV_h(p_1)}{\tau_{1h}},\qquad B_R=\frac{S}{\tau_{2h}},\qquad
 D_t(i)=\frac{-U b_{ti}^2+\eta_t S\big(b_{ti}[hi]+V_i(p_t)\tau_{thi}\big)}
 {\tau_{th}\tau_{ti}\tau_{thi}},\qquad (\eta_1,\eta_2)=(1,-1).
$$

To obtain the formula, split the ordered line at its middle emission or at the contact joining two neighboring emissions in the second-order Dirac current. Two same-helicity emissions on either side are already summed in $L_{ab}$ and $R_{cd}$. Each such pair has two internal orderings. This gives $1/4$ when both pairs occur and $1/2$ when only one does. The left current of a single positive photon vanishes since $V_i(p_1)=0$. Finally, inside the permutation sum $L_{ab} V_c(p_a+p_b)$ can be replaced by $2L_{ab} V_c(p_a)$. The analogous exchange of $c,d$ gives the factor of two multiplying $V_h(p_c)$. Every denominator displayed above is a physical fermion propagator. Expanding the formulas for these currents and the 24 permutations reproduces the 270 signed Laurent monomials of the original amplitude.

## AllSpurious

There is a shorter rule for writing the summand when spurious poles are allowed. Let $(i_1,i_2,i_3,i_4)=(a,b,c,d)$ and define the momentum along the fermion line before and after inserting the negative photon,
$$
 Q_j=p_1+\sum_{s=1}^j p_{i_s},\qquad
 D_j=Q_j^2-m^2,\quad \bar D_j=(Q_j+p_h)^2-m^2,\quad
 Z_j=\bar D_j-D_j=[h|Q_j|h\rangle .
$$
Here $Q_0=p_1$, $D_0=\bar D_4=0$, $Z_0=\tau_{1h}$ and $Z_4=-\tau_{2h}$. Set $x_j=[i_j|Q_{j-1}|h\rangle$ and take an empty product to be one. The four insertions share a common factor:
$$
K=\left(\prod_{s=1}^{4}\frac{x_s}{\langle h i_s\rangle}\right)
\sum_{j=1}^{4}
\frac{\langle i_j h\rangle\big(S[h i_j]-U x_j\big)}
 {Z_{j-1}Z_j\,
  \displaystyle\prod_{s=1}^{j-1}D_s\,
  \displaystyle\prod_{s=j}^{3}\bar D_s}.
$$
Only three interior $Z_j$ occur. If one vanishes, its two neighboring terms must be combined before taking that limit. The native numerical export uses the elementary amplitude. The displayed form keeps the Ward cancellation visible.

Start from the [one-negative ordered-line construction](../one-minus/README.md). For the derivation, choose $h$ as the common chiral reference for the four positives. Each positive orbital numerator is independent of where $h$ is inserted since $[i|p_h|h\rangle=0$. For an arbitrary square reference $r$ for the negative photon, its insertion at position $k$ has numerator $H_k=S+U[r|Q_k|h\rangle/[rh]$ and denominator factor $T_k=(\prod_{s=1}^k D_s\prod_{s=k}^3\bar D_s)^{-1}$. Write $J_k=(\prod_{s=1}^k D_s\prod_{s=k+1}^3\bar D_s)^{-1}$ for $0\leq k<4$, with $J_{-1}=J_4=0$. Then $Z_k T_k=J_k-J_{k-1}$. Summing by parts turns five insertion positions into four differences $J_{j-1}(H_{j-1}/Z_{j-1}-H_j/Z_j)$. Schouten removes $r$ from each difference and gives the numerator above. This derivation uses only the longitudinal Ward identity on the ordered fermion line and the external mass shells.

### Reducing the momentum insertions further

At either end of the ordered line three consecutive positives can be summed before joining them to the marked emission. This yields the lower-weight summand used by the native constructor:
$$
K=\sum_{j=1}^4 {\cal J}_1(i_1,\ldots,i_{j-1})
                 {\cal J}_2(i_4,\ldots,i_{j+1})
       \frac{x_j\big(Ux_j-S[h i_j]\big)}{Z_{j-1}Z_j}.
$$
Read each list from the massive endpoint toward the marked emission. With $\sigma_1=1$, $\sigma_2=-1$, use the following representatives under permutations of the legs on each side:
$$
\begin{aligned}
\ell_t(a)&=\frac{\sigma_t[a|p_t|h\rangle}{\langle h a\rangle\,\tau_{ta}},&
C_t(ab)&=-\frac{m^2[ab]^2}{\tau_{ta}\tau_{tb}\tau_{tab}},&
k_t(c,a)&=-\frac{\sigma_t[ca][c|p_t|a\rangle}{\tau_{tc}},\\
{\cal J}_t(\varnothing)&=1,& {\cal J}_t(a)&=\ell_t(a),\\
{\cal J}_t(a,b)&=\tfrac12\big(\ell_t(a)\ell_t(b)+C_t(ab)\big),\\
{\cal J}_t(a,b,c)&=
\frac{\ell_t(a)\ell_t(b)\ell_t(c)}{6}
+\frac{C_t(ab)}{2}\left(\ell_t(c)+\frac{2 k_t(c,a)}{\tau_{tabc}}\right).
\end{aligned}
$$
The product of three single emissions occurs in all six orders. Subtracting it from the summed current leaves a two-emission determinant proportional to $m^2[ab]^2$. Exchanging the two members of the pair gives equal contributions to the recoil term. The factors $1/6$ and $1/2$ account for those multiplicities in the outer permutation sum. The factors $Z_{j-1}$, $Z_j$ and $x_j$ depend only on the set of legs before the marked emission, so this replacement can be made on either side. For a single chosen ordering the two displayed forms of $K$ may have different values. Their signed sums over all 24 orderings agree.

## Complexity and signed checks

| Native representation | AllPhysical | AllSpurious |
| --- | ---: | ---: |
| Weighted complexity of one permutation summand | 331 | 408 |
| Leaves of that summand, including all current definitions in full | 878 | 1,084 |
| Weighted complexity of the full elementary amplitude | 3,966 | 4,634 |
| Leaves of the full elementary amplitude | 7,147 | 8,255 |

The displayed physical formula with named currents and the automatic physical summand both expand to the same 270 signed Laurent monomials. The shorter-to-write spurious formula with the common product has weight 524 when written in native operators. Summing the two- and three-photon currents first lowers that weight to 408.

For each pole choice, the native constructor passed all 30 physical residue checks, the spurious-pole, reference-spinor and fixed-angle high-energy checks, and 100,000 signed comparisons with Feynman diagrams. As a separate check, we evaluated each automatic summand at all 24 relabelings of each of 100,000 phase-space points and compared the complex sum to the elementary amplitude. The largest relative difference for AllPhysical was $3.72\times10^{-9}$. For AllSpurious, 2,193 points exceeded $10^{-8}$ in native arithmetic. The worst relative difference was $0.960$ before replay. Reconstructing those same points at 160 bits reduced the maximum to $4.27\times10^{-43}$. The short Ward formula with weight 524 required 1,599 replays on the same sample, with a native maximum of $5.31\times10^{-5}$. The export to SPINAS uses the elementary amplitude. The [existing CalcHEP and MadGraph comparisons](../qed-seven-point-external.tsv) cover that export.

All 40 QED constructions through six points met their existing complexity targets and passed their 10,000-point comparisons. All 47 native tests passed, including both helicity orientations and interleaved fermion and photon labels in the ordered-current test. Applying the generic permutation collector to 392 archived SM four-point expressions and 42 archived QED expressions produced ten compact forms. Expanding their permutations independently reproduced every signed coefficient.

## Reproduce

With the native driver built in `build/native`, run from the HEPCAT checkout

```sh
build/native/hepcat-qed --family eeAAAAA --helicity '++++-' --skip-formulas \
  --construct-only --points 100000 --seed 134791 --workers 1
```

Open `physical/eeAAAAA/++++-.txt` and `spurious/eeAAAAA/++++-.txt` in the reported output directory for the automatic summands. Their neighboring LaTeX files can be included in a paper. The explicit native amplitudes and the validation results are in the same run directory.

To recover the permutation sum from a saved native expression, read the contents of its `.expr` file into a string and use the external masses with their original model scope:

```cpp
const auto m = vertices.particle("e").mass;
hepcat::amp_expression amp(expression_text, {m,m,"0","0","0","0","0"});
amp.reduce();
std::cout << amp.plain_text() << '\n';
```

Standalone reduction recovered summand weights 331 and 408 from the two saved elementary amplitudes. It inferred the helicities and the reference leg from the expression. It checked proportionality to the reconstructed current using exact coefficients of the spinor-chain and propagator monomials. The expression and external masses were the only inputs.

With seed 134791 each recovered sum agreed with its saved amplitude at 100,000 signed phase-space points under the same replay rule. The physical sum also reproduced all 270 signed Laurent coefficients by exact expansion. The reloaded physical summand has 878 leaves and the reloaded spurious summand has 1,086. Equivalent factor orderings in the constructor's spurious summand save two leaves at the same weight.
