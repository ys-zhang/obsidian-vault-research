Let $d_i = \sum_j A_{ij}$ be the degree of node $i$ and adjacent matrix $A$.
The ***unnormalized graph Laplacian***, or [[Laplacian Matrix]]
$$
	L = D - A
$$


- $L$ 的最小特征值为$0$, 且**图$G$的连通分支个数等于$0$的特征子空间的位数**。
	$$L\textbf1 =0 =0\cdot \textbf 1$$
	$$0=\lambda = \frac{v^TLv}{v^Tv} = \frac{\sum_{i\sim j} (v_i-v_j)^2}{v^Tv}  $$
	thus 
	$$i\sim j \implies v_i=v_j$$


![[fiedler-theory.png]]
	
