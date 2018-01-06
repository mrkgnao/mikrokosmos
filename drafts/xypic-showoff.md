---
title: "Xy-pic examples"
date: 2017-11-11
---

# Commutative diagrams

## Function composition

$$
\xymatrix{
   A \ar[r] ^{f} 
     \ar[dr]_{g\circ f} 
 & B \ar[d] ^{g} \\
 & C
}
$$

```LaTeX
\xymatrix{
   A \ar[r] ^{f} 
     \ar[dr]_{g\circ f} 
 & B \ar[d] ^{g} \\
 & C
}
```

## A few linked exact sequences

Taken from [TeX.SE](https://tex.stackexchange.com/questions/176549/circling-a-collection-objects-in-xymatrix).

$$
\xymatrix{
  0 \ar[r] 
    & \Omega^0_X \ar[d] \ar[r] 
    & \Omega_X^1 \ar[r] \ar[d] 
    & \ldots \ar[r] 
    & \Omega_X^n \ar[d] \ar[r] 
    & 0 \\ 
  0 \ar[r] 
    & C^0(\Omega^0_X) \ar[d] \ar[r] 
    & C^0(\Omega_X^1) \ar[r] \ar[d] 
    & \ldots \ar[r] 
    & C^0(\Omega_X^n) \ar[d] \ar[r] 
    & 0 \\
  0 \ar[r] 
    & C^1(\Omega^0_X) \ar[d] \ar[r] 
    & C^1(\Omega_X^1) \ar[r] \ar[d] 
    & \ldots \ar[r] 
    & C^1(\Omega_X^n) \ar[d] \ar[r] 
    & 0 \\
  & \vdots 
  & \vdots                        
  & \ldots        
  & \vdots                        
  & \\
}
$$

```LaTeX
\xymatrix{
  0 \ar[r] 
    & \Omega^0_X \ar[d] \ar[r] 
    & \Omega_X^1 \ar[r] \ar[d] 
    & \ldots \ar[r] 
    & \Omega_X^n \ar[d] \ar[r] 
    & 0 \\ 
  0 \ar[r] 
    & C^0(\Omega^0_X) \ar[d] \ar[r] 
    & C^0(\Omega_X^1) \ar[r] \ar[d] 
    & \ldots \ar[r] 
    & C^0(\Omega_X^n) \ar[d] \ar[r] 
    & 0 \\
  0 \ar[r] 
    & C^1(\Omega^0_X) \ar[d] \ar[r] 
    & C^1(\Omega_X^1) \ar[r] \ar[d] 
    & \ldots \ar[r] 
    & C^1(\Omega_X^n) \ar[d] \ar[r] 
    & 0 \\
  & \vdots 
  & \vdots                        
  & \ldots        
  & \vdots                        
  & \\
}
```

## Tons of arrows!

$$
\xymatrix{
    \bullet\ar@{->}[rr]     && \bullet\\
    \bullet\ar@{.<}[rr]     && \bullet\\
    \bullet\ar@{~)}[rr]     && \bullet\\
    \bullet\ar@{=(}[rr]     && \bullet\\
    \bullet\ar@{~/}[rr]     && \bullet\\
    \bullet\ar@{^{(}->}[rr] && \bullet\\
    \bullet\ar@2{->}[rr]    && \bullet\\
    \bullet\ar@3{->}[rr]    && \bullet\\
    \bullet\ar@{=+}[rr]     && \bullet }
$$

## Vertical uses of cofibration arrows

$$
\newdir{ >}{{}*!/-5pt/@{>}}
\xymatrix{
  S_{P} 
    \ar@{=}[r] 
    \ar@{ >->}[d] & S_{P} 
    \ar@{->>}[r] 
    \ar@{ >->}[d] & 0 
    \ar@{ >->}[d] \\
  R' 
    \ar@{ >->}[r] 
    \ar@{->>}[d] & R 
    \ar@{->>}[r] 
    \ar@{->>}[d] & M 
    \ar@{=}[d] \\ 
  P' 
    \ar@{ >->}[r] & P 
    \ar@{->>}[r] & M
}
$$

```LaTeX
\newdir{ >}{{}*!/-5pt/@{>}}
\xymatrix{
  S_{P} 
    \ar@{=}[r] 
    \ar@{ >->}[d] & S_{P} 
    \ar@{->>}[r] 
    \ar@{ >->}[d] & 0 
    \ar@{ >->}[d] \\
  R' 
    \ar@{ >->}[r] 
    \ar@{->>}[d] & R 
    \ar@{->>}[r] 
    \ar@{->>}[d] & M 
    \ar@{=}[d] \\ 
  P' 
    \ar@{ >->}[r] & P 
    \ar@{->>}[r] & M
}
```

## The snake lemma!

$$
\newcommand\Ker{\mathsf{ker}\,}
\newcommand\Coker{\mathsf{coker}\,}
\begin{xy}
\xymatrix {
  0 \ar@[red][r] 
  & {\Ker f} \ar@[red][r] 
  & {\Ker a} \ar@[red][r] \ar[d] 
  & {\Ker b} \ar@[red][r] \ar[d] 
  & {\Ker c} \ar@[red]@`{[]+/r10pc/, [dddll]+/l10pc/}[dddll]_(0.55)d \ar[d]
\\
  & 0 \ar[r]
  & A \ar@[blue][r]^f \ar@[blue][d]^a 
  & B \ar@[blue][r] \ar@[blue][d]^b 
  & C \ar@[blue][r] \ar@[blue][d]^c 
  & 0
\\
  & 0 \ar@[blue][r] 
  & A' \ar@[blue][r] \ar[d] 
  & B' \ar@[blue][r]^{g'} \ar[d] 
  & C' \ar[r] \ar[d]
  & 0
\\
  & 
  & {\Coker a} \ar@[red][r] 
  & {\Coker b} \ar@[red][r] 
  & {\Coker c} \ar@[red][r] 
  & {\Coker g'} \ar@[red][r] 
  & 0
}
\end{xy}
$$

```LaTeX
\newcommand\Ker{\mathsf{ker}\,}
\newcommand\Coker{\mathsf{coker}\,}
\begin{xy}
\xymatrix {
  0 \ar@[red][r] 
  & {\Ker f} \ar@[red][r] 
  & {\Ker a} \ar@[red][r] \ar[d] 
  & {\Ker b} \ar@[red][r] \ar[d] 
  & {\Ker c} \ar@[red]@`{[]+/r10pc/, [dddll]+/l10pc/}[dddll]_(0.55)d \ar[d]
\\
  & 0 \ar[r]
  & A \ar@[blue][r]^f \ar@[blue][d]^a 
  & B \ar@[blue][r] \ar@[blue][d]^b 
  & C \ar@[blue][r] \ar@[blue][d]^c 
  & 0
\\
  & 0 \ar@[blue][r] 
  & A' \ar@[blue][r] \ar[d] 
  & B' \ar@[blue][r]^{g'} \ar[d] 
  & C' \ar[r] \ar[d]
  & 0
\\
  & 
  & {\Coker a} \ar@[red][r] 
  & {\Coker b} \ar@[red][r] 
  & {\Coker c} \ar@[red][r] 
  & {\Coker g'} \ar@[red][r] 
  & 0
}
\end{xy}
```
