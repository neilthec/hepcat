# Five positive-helicity photons and a massive fermion pair

Label the incoming electron and positron 1 and 2. Let $(a,b,c,d,k)$ run over the 120 permutations of the five photon labels $(3,4,5,6,7)$. The AllPhysical amplitude can be written with three terms inside this sum.

$$
\begin{aligned}
\mathcal M
={}&m(\sqrt{2}e)^5\langle\mathbf1\mathbf2\rangle
\sum_{(a,b,c,d,k)}
\frac{[ab]^2}{12\,\tau_{1k}\tau_{1a}\tau_{1b}\tau_{2c}\tau_{2d}}\\[-2pt]
&\quad\times\left[
\frac{X_c X_d X_k}{\tau_{1c}\tau_{1d}\tau_{2k}}
-\frac{3m^2[cd]^2}{\tau_{1ab}}
\left(\frac{X_k}{\tau_{2k}}+\frac{2Y_{ka}}{\tau_{2cd}}\right)
\right].
\end{aligned}
$$

Here $m$ is the electron mass, $X_i=[i12i]$ and $Y_{ij}=[ij][i1j\rangle$. Numerals inside a chain denote momentum insertions. For example, $[i12i]=[i|p_1p_2|i]$ and $[i1j\rangle=[i|p_1|j\rangle$. Each subscript of $\tau$ lists every leg flowing through that propagator, so $\tau_{1a}=(p_1+p_a)^2-m^2$ and $\tau_{2cd}=(p_2+p_c+p_d)^2-m^2$. The bold labels retain the two external massive spin indices.

The first pattern has ten distinct images. It selects an unordered pair of photons out of five, and the other three enter symmetrically. Each image therefore occurs twelve times. The second pattern has 30 images, each occurring four times, and the third has 60 images, each occurring twice. Their coefficients in the permutation summand are respectively $1/12$, $-1/4$, and $-1/2$ after the common dimensional factors have been extracted. These factors count repeated terms within the **amplitude**. Scattering symmetry factors enter separately in a rate.

The same reduction gives a second summand for AllSpurious. In this formula the displayed labels are the representative and the permutation acts on every occurrence of a photon label. Write $C=m(\sqrt{2}e)^5\langle\mathbf1\mathbf2\rangle$ and $B=(m[34])^2$. Then
$$
\mathcal M = C\sum_{\pi\in S_{\{3,4,5,6,7\}}}\pi\!\left\{
\frac{[56]}{6\,\tau_{13}\tau_{14}\tau_{26}\langle67\rangle}
\left[
\frac{[716\rangle}{\tau_{17}\langle56\rangle}
\left(\frac{3B}{\tau_{256}}-
\frac{[416\rangle[316\rangle}{\langle36\rangle\langle46\rangle}\right)
+\frac{3B}{\tau_{256}\tau_{134}}
\left([57]-\frac{[726\rangle}{\langle56\rangle}\right)
\right]\right\}.
$$

| Quantity | AllPhysical | AllSpurious |
|---|---:|---:|
| Distinct terms in the fully distributed expression | 100 | 200 |
| Terms in one permutation summand | 3 | 4 |
| Distinct images of those terms | 10, 30, 60 | 20, 60, 60, 60 |
| Weighted complexity of one summand | 39 | 24 |
| Weighted complexity of the full expression | 820 | 920 |
| Leaves of the explicit expression before fraction collection | 4,272 | 7,900 |
| Leaves of the explicit expression after fraction collection | 4,226 | 7,853 |

The new fraction rule collects terms over a shared propagator while keeping the regular contributions. It factors the resulting numerator if this saves further leaves. The permutation rule then checks the coefficient, spinor orientation and propagator labels of every term before shortening the human-readable output. Several equivalent choices of representative can give the same sum. A separate search applied one or two spinor identities to representatives and summed their signed images. It found no lower full weight among 768 AllPhysical and 268 AllSpurious alternatives. The search does not establish a global minimum.

## Reproduce in C++

From the HEPCAT checkout with the native driver built in `build/native`, run

```sh
build/native/hepcat-qed --family eeAAAAA --helicity '+++++' --skip-formulas \
  --construct-only --points 100000 --seed 134791 --workers 1
```

The driver prints the results directory. Open `physical/eeAAAAA/+++++.txt` or `spurious/eeAAAAA/+++++.txt` there for the permutation form and its adjacent `.tex` file for the LaTeX. The native `.expr` file gives the complete elementary expression used for evaluating the amplitude. For each representation the construction passed all 30 physical residue checks, the reference-spinor, spurious-pole and fixed-angle high-energy checks, and the signed Feynman comparison at 100,000 points. An exact Laurent expansion of the two formulas above, summed over all 120 relabelings, reproduces every signed coefficient of the earlier elementary amplitudes. The native permutation regression separately evaluates the compact summands at all 120 relabelings, retaining the complex phase.

For regression coverage, all 40 native QED constructions through six points passed at 10,000 points per comparison and matched or improved their former weighted scores. Replaying fraction collection on 392 saved SM four-point expressions and 42 saved QED expressions shortened 31 SM and 16 QED expressions. Exact expansion reproduced each original expression, including its model coefficients.
