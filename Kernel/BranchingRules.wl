(* ::Package:: *)
(* :Title: LieART`BranchingRules *)
(* :Authors: Robert Feger, Thomas Kephart and Robert Saskowski *)
(* :Summary: *)
(* :Context:   LieART`BranchingRules` *)
(* :Package version:   2.1.1 *)
(* :Copyright:  Copyright 2012-2025, Robert Feger, Thomas Kephart and Robert Saskowski. *)

(*This file is part of LieART. See LieART.m for licensing information*) 

BeginPackage["LieART`"]

{U,A,B,C,D,F4,G2,E6,E7,E8,
Algebra,ProductAlgebra,
ProductIrrep,Irrep,
SpecialSubalgebra,
NonSemiSimpleSubalgebra,
NonMaximalSubalgebra,
SemiSimpleSubalgebra,
ProjectAlgebra,
ReverseOrderSimpleRoots}

ProjectionMatrix::usage = "ProjectionMatrix  "

EndPackage[]


BeginPackage["LieART`BranchingRules`",{"LieART`"}]


Begin["`Private`"]

(*many of the explicit forms of branching rules were obtained from:
"Finite-Dimensional Lie Algebras and Their Representations for Unified Model Building"
N. Yamatsu (2015) arXiv:1511.08771
-and-
"Generalized projection matrices for nonsupersymmetric and supersymmetric grand unified theories"
C. Kim, I. Koh, Y. Park, K.Y. Kim, Y. Kim. (1983) Phys.Rev. D 27.
*)

(* ::Section:: *)
(* Regular Subalgebras *)

(* ::Subsection:: *)
(* Semi-Simple Subalgebras *)

(* ::Subsubsection:: *)
(* A_n *)

(* ::Subsubsection:: *)
(* B_n *)
ProjectionMatrix[origin:Algebra[B][3] ,ProductAlgebra[Algebra[A][1],Algebra[A][1],Algebra[A][1]]]    := SemiSimpleSubalgebra[origin, 2]
ProjectionMatrix[origin:Algebra[B][3] ,ProductAlgebra[Algebra[A][3]]]                                := Permute[SemiSimpleSubalgebra[origin, 3],Cycles[{{2,3}}]]
ProjectionMatrix[origin:Algebra[B][4],ProductAlgebra[Algebra[A][3],Algebra[A][1]]]                   := Permute[SemiSimpleSubalgebra[origin, 3],Cycles[{{2,3}}]]
ProjectionMatrix[origin:Algebra[B][5] ,ProductAlgebra[Algebra[A][3],Algebra[C][2]]] 	 := Permute[{{0,1,0,0,0},{1,0,0,0,0},{-1,-2,-2,-2,-1},{0,0,0,0,1},{0,0,0,1,0}},Cycles[{{1,2}}]]
ProjectionMatrix[origin:Algebra[B][n_],ProductAlgebra[Algebra[A][3],Algebra[B][m_]]]                 := Permute[SemiSimpleSubalgebra[origin, 3],Cycles[{{2,3}}]] /; m==(n-3)&&n>=6
ProjectionMatrix[origin:Algebra[B][n_],ProductAlgebra[Algebra[D][n_]]]                               := ReverseOrderSimpleRoots[SemiSimpleSubalgebra[origin, -1],n] /; n>=4
ProjectionMatrix[origin:Algebra[B][n_],ProductAlgebra[Algebra[A][1],Algebra[A][1],Algebra[B][m_]]] := 
	Module[{mat},
	  mat = Join[{1}, ConstantArray[0, n - 1]];
	  mat = {mat, Join[{-1}, ConstantArray[-2, n - 2], {-1}]};
	  mat = Join[mat, 
	    Join[ConstantArray[0, {n - 2, 2}], IdentityMatrix[n - 2], 2]];
	  mat
	] /; n >= 5 && m==n-2
ProjectionMatrix[origin:Algebra[B][4],ProductAlgebra[Algebra[A][1],Algebra[A][1],Algebra[C][2]]] 	 := Permute[SemiSimpleSubalgebra[origin,2],Cycles[{{3,4}}]]
ProjectionMatrix[origin:Algebra[B][n_],ProductAlgebra[Algebra[D][k_],Algebra[B][m_]]] 				 :=
	Module[{mat},
	  mat = Join[Reverse[IdentityMatrix[k - 1]], 
	    ConstantArray[0, {k - 1, n - k + 1}], 2];
	  AppendTo[mat, Join[{-1}, ConstantArray[-2, n - 2], {-1}]];
	  mat = Join[mat, 
	    Join[ConstantArray[0, {n - k, k}], IdentityMatrix[n - k], 2]];
	  mat
	] /; k >= 4 && k <= n && n - k >= 3 && m==n-k
ProjectionMatrix[origin:Algebra[B][n_],ProductAlgebra[Algebra[D][m_],Algebra[A][1]]] 				 := ReverseOrderSimpleRoots[SemiSimpleSubalgebra[origin,n-1],n-1] /; m==(n-1) && n>=5
ProjectionMatrix[origin:Algebra[B][n_],ProductAlgebra[Algebra[D][k_],Algebra[C][2]]] 				 :=
	Permute[
		Module[{mat},
			mat = Join[Reverse[IdentityMatrix[k - 1]], 
	    	ConstantArray[0, {k - 1, n - k + 1}], 2];
	  		AppendTo[mat, Join[{-1}, ConstantArray[-2, n - 2], {-1}]];
	  		mat = Join[mat, 
	    	Join[ConstantArray[0, {n - k, k}], IdentityMatrix[n - k], 2]];
	  		mat
		],
		Cycles[{{n,n-1}}]
	] /; k>=4 && (n-k)==2

(* ::Subsubsection:: *)
(* C_n *)
ProjectionMatrix[origin:Algebra[C][n_],ProductAlgebra[Algebra[A][1],Algebra[C][m_]]] 				 := Join[{ConstantArray[1, n]}, MapThread[Prepend, {IdentityMatrix[n - 1], ConstantArray[0, n - 1]}]] /; n >= 3&&m==n-1
ProjectionMatrix[origin:Algebra[C][2],ProductAlgebra[Algebra[A][1],Algebra[A][1]]] := {{1,1},{0,1}}
ProjectionMatrix[origin:Algebra[C][n_],ProductAlgebra[Algebra[C][k_],Algebra[C][m_]]] 				 :=
	Module[{mat},
	  mat = Join[IdentityMatrix[k - 1], ConstantArray[0, {k - 1, n - k + 1}], 2];
	  AppendTo[mat, Join[ConstantArray[0, k - 1], ConstantArray[1, n - k + 1]]];
	  mat = Join[mat, Join[ConstantArray[0, {n - k, k}], IdentityMatrix[n - k], 2]];
	  mat
	  ] /; n >= 3 && k >= 2 && k <= n - 2 && m==n-k && m>=k

(* ::Subsubsection:: *)
(* D_n *)
ProjectionMatrix[origin:Algebra[D][4] ,ProductAlgebra[Algebra[A][1],Algebra[A][1],Algebra[A][1],Algebra[A][1]]]   := {{1,0,0,0},{-1,-2,-1,-1},{0,0,1,0},{0,0,0,1}}
ProjectionMatrix[origin:Algebra[D][5] ,ProductAlgebra[Algebra[A][1],Algebra[A][1],Algebra[A][3]]]   := Permute[SemiSimpleSubalgebra[origin, 2],Cycles[{{3,4}}]]
ProjectionMatrix[origin:Algebra[D][6] ,ProductAlgebra[Algebra[A][3],Algebra[A][3]]]   	:= {{1,0,0,0,0,0},{0,1,0,0,0,0},{-1,-2,-2,-2,-1,-1},{0,0,0,0,1,0},{0,0,0,1,0,0},{0,0,0,0,0,1}}
ProjectionMatrix[origin:Algebra[D][n_] ,ProductAlgebra[Algebra[A][1],Algebra[A][1],Algebra[D][m_]]] := SemiSimpleSubalgebra[origin, 2] /; m==(n-2)&&n>=3
ProjectionMatrix[origin:Algebra[D][n_] ,ProductAlgebra[Algebra[D][m_],Algebra[D][k_]]]              := ReverseOrderSimpleRoots[SemiSimpleSubalgebra[origin, m],m] /; m==(n-k)&&n>=6
ProjectionMatrix[origin:Algebra[D][n_] ,ProductAlgebra[Algebra[A][3],Algebra[D][k_]]]  := Permute[SemiSimpleSubalgebra[origin,3],Cycles[{{2,3}}]] /; n >= 7 && k==n-3

(* ::Subsubsection:: *)
(* E_6 *)
ProjectionMatrix[origin:E6 ,ProductAlgebra[Algebra[A][2],Algebra[A][2],Algebra[A][2]]] := SemiSimpleSubalgebra[origin, 3]
ProjectionMatrix[origin:E6 ,ProductAlgebra[Algebra[A][5],Algebra[A][1]]]               := SemiSimpleSubalgebra[origin, 6]

(* ::Subsubsection:: *)
(* E_7 *)
ProjectionMatrix[origin:E7,ProductAlgebra[Algebra[A][7]]]    := SemiSimpleSubalgebra[origin,7]
ProjectionMatrix[origin:E7,ProductAlgebra[Algebra[D][6],Algebra[A][1]]]    := Permute[SemiSimpleSubalgebra[origin,5],Cycles[{{5,6,7}}]]
ProjectionMatrix[origin:E7,ProductAlgebra[Algebra[A][5],Algebra[A][2]]]    := Permute[SemiSimpleSubalgebra[origin,4],Cycles[{{5,6,7}}]]

(* ::Subsubsection:: *)
(* E_8 *)
ProjectionMatrix[origin:E8,ProductAlgebra[Algebra[A][8]]]    := SemiSimpleSubalgebra[origin,8]
ProjectionMatrix[origin:E8,ProductAlgebra[E7,Algebra[A][1]]] := Permute[SemiSimpleSubalgebra[origin,7],Cycles[{{7, 8}}]]
ProjectionMatrix[origin:E8,ProductAlgebra[E6,Algebra[A][2]]] := Permute[SemiSimpleSubalgebra[origin,6],Cycles[{{6, 8}}]]
ProjectionMatrix[origin:E8,ProductAlgebra[Algebra[A][4],Algebra[A][4]]] := Permute[SemiSimpleSubalgebra[origin,4],Cycles[{{4, 5, 6, 7, 8}}]]
ProjectionMatrix[origin:E8,ProductAlgebra[Algebra[D][8]]]    := Permute[ReverseOrderSimpleRoots[SemiSimpleSubalgebra[origin,1],7],Cycles[{{7,8}}]]

(* ::Subsubsection:: *)
(* F_4 *)
ProjectionMatrix[origin:F4,ProductAlgebra[Algebra[B][4]]] := {{-2,-3,-2,-1},{1,0,0,0},{0,1,0,0},{0,0,1,0}}
ProjectionMatrix[origin:F4,ProductAlgebra[Algebra[A][2],Algebra[A][2]]] := Permute[SemiSimpleSubalgebra[origin,2],Cycles[{{1,4,3,2}}]]
ProjectionMatrix[origin:F4,ProductAlgebra[Algebra[A][1],Algebra[C][3]]] := {{-2,-3,-2,-1},{0,0,0,1},{0,0,1,0},{0,1,0,0}}

(* ::Subsubsection:: *)
(* G_2 *)
ProjectionMatrix[origin:G2, ProductAlgebra[Algebra[A][2]]] := SemiSimpleSubalgebra[origin,1]
ProjectionMatrix[origin:G2, ProductAlgebra[Algebra[A][1], Algebra[A][1]]] := SemiSimpleSubalgebra[origin,2]

(* ::Section:: *)
(* Non-Semi-Simple Subalgebras *)

(* ::Subsubsection:: *)
(* A_n *)
ProjectionMatrix[origin:Algebra[A][n_],ProductAlgebra[Algebra[A][m_],Algebra[A][k_],Algebra[U][1]]] := NonSemiSimpleSubalgebra[origin,-k-1] /; m==(n-k-1)
ProjectionMatrix[origin:Algebra[A][n_],ProductAlgebra[Algebra[A][m_],Algebra[U][1]]] := NonSemiSimpleSubalgebra[origin,-1] /; m==(n-1)&&n>=2
ProjectionMatrix[origin:Algebra[A][1], ProductAlgebra[Algebra[U][1]]] := NonSemiSimpleSubalgebra[origin,1]

(* ::Subsubsection:: *)
(* B_n *)
ProjectionMatrix[origin:Algebra[B][3] ,ProductAlgebra[Algebra[C][2],Algebra[U][1]]] := {{0,0,1},{0,1,0},{2,2,1}}
ProjectionMatrix[origin:Algebra[B][n_],ProductAlgebra[Algebra[B][m_],Algebra[U][1]]] := NonSemiSimpleSubalgebra[origin,1] /; m==(n-1)&&n>=3 (*originally -1*)

(* ::Subsubsection:: *)
(* C_n *)
ProjectionMatrix[origin:Algebra[C][n_],ProductAlgebra[Algebra[A][m_],Algebra[U][1]]] := NonSemiSimpleSubalgebra[origin,-1] /; m==(n-1)&&n>=2

(* ::Subsubsection:: *)
(* D_n *)
ProjectionMatrix[origin:Algebra[D][n_],ProductAlgebra[Algebra[D][m_],Algebra[U][1]]] := NonSemiSimpleSubalgebra[origin,1] /; m==(n-1)&&n>=5 (*originally -2*)
(*ProjectionMatrix[origin:Algebra[D][n_],ProductAlgebra[Algebra[A][m_],Algebra[U][1]]] := NonSemiSimpleSubalgebra[origin,-2] /; m==(n-1)&&n>=4*)
ProjectionMatrix[origin:Algebra[D][5],ProductAlgebra[Algebra[A][4],Algebra[U][1]]] := {{0,0,0,1,0},{0,0,1,0,0},{0,1,0,0,0},{1,0,0,0,0},{-2,-4,-6,-3,-5}}
ProjectionMatrix[origin:Algebra[D][n_],ProductAlgebra[Algebra[A][m_],Algebra[U][1]]] := Join[Transpose@Join[IdentityMatrix[n-1], {ConstantArray[0,n-1]}],{Join[Range[n-2],{(n-2)/2,n/2}]}]/; m==(n-1)&&n>=4&&EvenQ[n]&&n<=10
ProjectionMatrix[origin:Algebra[D][n_],ProductAlgebra[Algebra[A][m_],Algebra[U][1]]] := Join[Transpose@Join[IdentityMatrix[n-1], {ConstantArray[0,n-1]}],{Join[2*Range[n-2],{n-2,n}]}]/; m==(n-1)&&n>=4

(* ::Subsubsection:: *)
(* E_6 *)
ProjectionMatrix[origin:E6,ProductAlgebra[Algebra[D][5],Algebra[U][1]]] := NonSemiSimpleSubalgebra[origin,5]

(* ::Subsubsection:: *)
(* E_7 *)
ProjectionMatrix[origin:E7,ProductAlgebra[E6,Algebra[U][1]]] := NonSemiSimpleSubalgebra[origin,6]

(* ::Subsection:: *)
(* Non-Maximal Subalgebras *)

(* ::Subsubsection:: *)
(* A_n *)
(*ProjectionMatrix[origin:Algebra[A][n_],ProductAlgebra[Algebra[A][m_],Algebra[A][k_]]] := NonMaximalSubalgebra[origin,{m+1}] /; m==(n-k-1)&&n>=3
ProjectionMatrix[origin:Algebra[A][n_],ProductAlgebra[Algebra[A][m_]]] := NonMaximalSubalgebra[origin,{m+1,n}] /; m<n&&n>=2*)

(* ::Subsubsection:: *)
(* B_n *)

(* ::Subsubsection:: *)
(* C_n *)

(* ::Subsubsection:: *)
(* D_n *)

(* ::Section:: *)
(* Special Subalgebras *)

(* ::Subsection:: *)
(* A_n *)
ProjectionMatrix[origin:Algebra[A][2],ProductAlgebra[Algebra[A][1]]] := SpecialSubalgebra[origin,{ProductIrrep[Irrep[A][2]]}]
ProjectionMatrix[origin:Algebra[A][5],ProductAlgebra[Algebra[A][3]]] := {{0,1,0,1,0},{1,0,0,0,1},{0,1,2,1,0}}
ProjectionMatrix[origin:Algebra[A][5],ProductAlgebra[Algebra[A][2]]] := {{0,1,0,2,2}, {2,2,3,1,0}} (*tried {{1,2}} but got conjugates wrong*)
ProjectionMatrix[origin:Algebra[A][6],ProductAlgebra[Algebra[B][3]]] :=  SpecialSubalgebra[origin,{ProductIrrep[Irrep[B][1,0,0]]}]
ProjectionMatrix[origin:Algebra[A][m_],ProductAlgebra[Algebra[B][n_]]] :=  
	Module[{p, bottom},
		p = IdentityMatrix[n - 1];
		p = MapThread[Append, {p, ConstantArray[0, n - 1]}];
		p = MapThread[Append, {p, ConstantArray[0, n - 1]}];
		p = Join[p, Reverse[IdentityMatrix[n - 1]], 2];
		bottom = ConstantArray[0, n - 1];
		AppendTo[bottom, 2];
		AppendTo[bottom, 2];
		bottom = Join[bottom, ConstantArray[0, n - 1]];
		AppendTo[p, bottom];
		Return[p]
	  ] /; n>=3 && m==2*n
ProjectionMatrix[origin:Algebra[A][m_],ProductAlgebra[Algebra[C][n_]]] := 
	Module[{p, bottom},
		p = IdentityMatrix[n - 1];
		p = MapThread[Append, {p, ConstantArray[0, n - 1]}];
		p = Join[p, Reverse[IdentityMatrix[n - 1]], 2];
		bottom = ConstantArray[0, n - 1];
		AppendTo[bottom, 1];
		bottom = Join[bottom, ConstantArray[0, n - 1]];
		AppendTo[p, bottom];
		Return[p]
	   ] /; n>=2 && m==2*n-1
ProjectionMatrix[origin:Algebra[A][m_],ProductAlgebra[Algebra[D][n_]]] := 
	Module[{p, bottom},
		p = IdentityMatrix[n - 1];
		p = MapThread[Append, {p, ConstantArray[0, n - 1]}];
		p = Join[p, Reverse[IdentityMatrix[n - 1]], 2];
		bottom = ConstantArray[0, n - 2];
		AppendTo[bottom, 1];
		AppendTo[bottom, 2];
		AppendTo[bottom, 1];
		bottom = Join[bottom, ConstantArray[0, n - 2]];
		AppendTo[p, bottom];
		Return[p]
	   ] /; n>=4 && m==2*n-1
ProjectionMatrix[origin:Algebra[A][r_],ProductAlgebra[Algebra[A][s_],Algebra[A][t_]]] :=
	Module[{i, mat},
		mat = IdentityMatrix[s];
		For[i = 2, i <= t + 1, i++,
			mat = MapThread[Append, {mat, ConstantArray[0, s]}];
			mat = Join[mat, IdentityMatrix[s], 2];
		];
		For[i = 1, i <= t, i++,
			AppendTo[mat,
				Join[ConstantArray[0, (i - 1)*(s + 1)],
					Range[s + 1],
					Reverse[Range[s]],
					ConstantArray[0, s*t + s + t - (2*s + 1) - (i - 1)*(s + 1)]
				]
			];
	  	];
	  	Return[mat]
	 ] /; (s>=t && r==s*t+s+t)
ProjectionMatrix[origin:Algebra[A][4],ProductAlgebra[Algebra[C][2]]] := SpecialSubalgebra[origin, {ProductIrrep[Irrep[C][0,1]]}]
ProjectionMatrix[origin:Algebra[A][9],ProductAlgebra[Algebra[A][2]]] := {{3,4,3,5,2,2,0,1,0},{0,1,3,2,5,5,6,4,3}}
ProjectionMatrix[origin:Algebra[A][9],ProductAlgebra[Algebra[A][3]]] := {{2,2,3,1,0,1,1,0,0},{0,1,0,2,2,2,0,1,0},{0,0,1,1,2,1,3,2,2}}
ProjectionMatrix[origin:Algebra[A][9],ProductAlgebra[Algebra[A][4]]] := {{0,1,0,1,0,1,1,0,0},{1,0,0,0,1,1,0,1,0},{0,1,2,1,0,0,0,0,1},{0,0,0,1,2,1,2,1,0}}
ProjectionMatrix[origin:Algebra[A][14],ProductAlgebra[Algebra[A][2]]] := {{2,5,5,6,7,5,7,6,5,5,5,2,3,1},{1,0,2,2,2,5,3,4,5,4,3,5,2,2}}
(**)
(*Multiple SU15 -> SU3 Branching Rules:*)
(*1*) ProjectionMatrix[origin:Algebra[A][14],ProductAlgebra[Algebra[A][2]],1] := {{2,5,5,6,7,5,7,6,5,5,5,2,3,1},{1,0,2,2,2,5,3,4,5,4,3,5,2,2}}
(*2*) ProjectionMatrix[origin:Algebra[A][14],ProductAlgebra[Algebra[A][2]],2] := {{4,6,9,9,10,8,10,9,5,5,2,3,1,0},{0,1,0,2,2,5,3,4,8,7,9,6,6,4}}
(**)
ProjectionMatrix[origin:Algebra[A][14],ProductAlgebra[Algebra[A][4]]] := {{2,2,3,1,0,1,0,1,1,1,0,0,0,0},{0,1,0,2,2,2,3,3,1,0,1,0,0,0},{0,0,1,1,2,1,0,0,2,2,2,3,1,0},{0,0,0,0,0,1,2,1,1,2,1,0,2,2}}
ProjectionMatrix[origin:Algebra[A][14],ProductAlgebra[Algebra[A][5]]] := {{0,1,0,1,0,1,1,0,1,1,0,0,0,0},{1,0,0,0,1,1,0,1,1,0,1,1,0,0},{0,1,2,1,0,0,0,0,0,1,1,0,1,0},{0,0,0,1,2,1,2,1,1,0,0,0,0,1},{0,0,0,0,0,1,1,2,1,2,1,2,1,0}}
ProjectionMatrix[origin:Algebra[A][15],ProductAlgebra[Algebra[D][5]]] := {{0,0,0,0,1,0,1,0,1,0,1,0,0,0,0},{0,0,1,2,1,1,0,0,0,1,1,2,1,0,0},{0,1,0,0,0,0,1,2,1,0,0,0,0,1,0},{0,0,1,0,1,2,1,0,0,0,0,0,0,0,1},{1,0,0,0,0,0,0,0,1,2,1,0,1,0,0}}
(*UNTESTED*)ProjectionMatrix[origin:Algebra[A][26],ProductAlgebra[E6]] := SpecialSubalgebra[origin, {ProductIrrep[Irrep[E6][1,0,0,0,0,0]]}]


(* ::Subsection:: *)
(* B_n *)
ProjectionMatrix[origin:Algebra[B][3] ,ProductAlgebra[G2]] := SpecialSubalgebra[origin, {ProductIrrep[Irrep[G2][0,0]],ProductIrrep[Irrep[G2][1,0]]}]
ProjectionMatrix[origin:Algebra[B][4] ,ProductAlgebra[Algebra[A][1],Algebra[A][1]]] := {{2,4,6,3},{2,2,0,1}}
ProjectionMatrix[origin:Algebra[B][7] ,ProductAlgebra[Algebra[A][3]]] :={{1,0,1,0,0,2,1},{0,1,2,4,3,2,1},{1,2,1,0,2,2,1}}
ProjectionMatrix[origin:Algebra[B][7] ,ProductAlgebra[Algebra[A][1],Algebra[C][2]]] := {{2,4,6,8,10,10,5},{0,2,2,0,0,0,1},{1,0,0,1,0,1,0}}
ProjectionMatrix[origin:Algebra[B][10] ,ProductAlgebra[Algebra[A][1],Algebra[B][3]]] := {{2,4,6,8,10,12,14,14,14,7},{1,0,0,0,0,1,0,1,0,0},{0,1,0,0,1,0,0,0,1,0},{0,0,2,2,0,0,0,0,0,1}}
ProjectionMatrix[origin:Algebra[B][10] ,ProductAlgebra[Algebra[B][3]]] := {{0,1,0,1,0,1,0,0,2,1},{1,0,0,0,1,2,4,3,2,1},{0,2,4,4,4,2,0,2,2,1}}
ProjectionMatrix[origin:Algebra[B][10] ,ProductAlgebra[Algebra[C][3]]] := {{2,2,0,1,0,1,0,2,2,1},{0,1,3,2,2,3,5,4,2,1},{0,0,0,1,2,1,0,0,2,1}}
ProjectionMatrix[origin:Algebra[B][12] ,ProductAlgebra[Algebra[C][2],Algebra[C][2]]] := {{0,0,0,0,0,2,4,6,8,10,10,5},{1,2,3,4,5,4,3,2,1,0,0,0},{0,2,2,0,0,0,2,2,0,0,0,1},{1,0,0,1,0,1,0,0,1,0,1,0}}
ProjectionMatrix[origin:Algebra[B][13] ,ProductAlgebra[Algebra[A][2]]] := {{2,5,5,9,7,8,9,11,13,12,11,14,7},{2,2,5,3,7,8,9,8,7,9,11,8,4}}
ProjectionMatrix[origin:Algebra[B][13] ,ProductAlgebra[Algebra[B][3]]] := {{2,2,3,1,2,1,2,1,1,3,2,2,1},{0,1,0,2,2,2,3,4,2,1,3,2,1},{0,0,2,2,2,4,2,2,6,6,4,6,3}}
ProjectionMatrix[origin:Algebra[B][13] ,ProductAlgebra[Algebra[A][1],Algebra[B][4]]] := {{2,4,6,8,10,12,14,16,18,18,18,18,9},{1,0,0,0,0,0,0,1,0,1,0,0,0},{0,1,0,0,0,0,1,0,0,0,1,0,0},{0,0,1,0,0,1,0,0,0,0,0,1,0},{0,0,0,2,2,0,0,0,0,0,0,0,1}}
ProjectionMatrix[origin:Algebra[B][13] ,ProductAlgebra[Algebra[C][4]]] := {{0,1,0,1,0,1,1,0,1,1,0,2,1},{1,0,0,0,1,1,0,1,2,1,3,2,1},{0,1,2,1,0,1,1,2,1,3,2,2,1},{0,0,0,1,2,1,2,1,1,0,0,0,0}}
ProjectionMatrix[origin:Algebra[B][n_] ,ProductAlgebra[Algebra[A][1]]] := 
	Module[{mat,k},
	   mat = {};
	   For[k = 1, k < n, k++,
	    AppendTo[mat, k*(2*n - k + 1)];
	    ];
	   AppendTo[mat, n*(n + 1)/2];
	   Return[{mat}]
	] /; n >= 4
(* B_[(mn-1)/2] -> B_(mn/2) x [B_(n-1)/2], ie SO(mn) -> SO(m) x SO(n) *)
ProjectionMatrix[origin:Algebra[B][p_] ,ProductAlgebra[Algebra[B][s_],Algebra[B][t_]]]:= 
	Module[{mat, bottom, q, b, r, row,m,n},
	  m=2*s+1;
	  n=2*t+1;
	  
	  q = Join[
	    IdentityMatrix[(n - 3)/2],
	    ConstantArray[0, {(n - 3)/2, 2}],
	    Reverse[IdentityMatrix[(n - 3)/2]], 2
	    ];
	  AppendTo[q, 
	   Join[ConstantArray[0, (n - 3)/2], {2, 2}, 
	    ConstantArray[0, (n - 3)/2]]];
	  q = MapThread[Append, {q, ConstantArray[0, (n - 1)/2]}];
	  
	  b = MapThread[
	    Append, {IdentityMatrix[(n - 3)/2], ConstantArray[0, (n - 3)/2]}];
	  AppendTo[b, Join[ConstantArray[0, (n - 3)/2], {1}]];
	  
	  bottom = q;
	  For[row = 2, row <= (m - 1)/2, row++,
	   bottom = Join[bottom, q, 2];
	   ];
	  bottom = Join[bottom, b, 2];
	  
	  mat = {};
	  For[row = 1, row <= (m - 1)/2 - 1, row++,
	   r = Join[
	     ConstantArray[0, n*(row - 1)],
	     Range[n],
	     Reverse[Range[n - 1]],
	     ConstantArray[0, (m*n - 1)/2 - n*(row - 1) - (2 n - 1)]
	     ];
	   AppendTo[mat, r];
	   ];
	  
	  AppendTo[mat, 
	   Join[ConstantArray[0, (m*n - 1)/2 - n - (n - 1)/2], 
	    Range[2, 2 n - 2, 2], ConstantArray[2 n, (n - 1)/2], {n}]];
	  mat = Join[mat, bottom];
	  
	  Return[mat]
	  ] /; s>=t && t>=2 && s>=3 && p==2*s*t+s+t
ProjectionMatrix[origin:Algebra[B][p_] ,ProductAlgebra[Algebra[B][s_],Algebra[C][2]]]:= 
	Module[{mat, bottom, q, b, r, row,m,n},
	  m=2*s+1;
	  n=2*2+1;
	  
	  q = Join[
	    IdentityMatrix[(n - 3)/2],
	    ConstantArray[0, {(n - 3)/2, 2}],
	    Reverse[IdentityMatrix[(n - 3)/2]], 2
	    ];
	  AppendTo[q, 
	   Join[ConstantArray[0, (n - 3)/2], {2, 2}, 
	    ConstantArray[0, (n - 3)/2]]];
	  q = MapThread[Append, {q, ConstantArray[0, (n - 1)/2]}];
	  
	  b = MapThread[
	    Append, {IdentityMatrix[(n - 3)/2], ConstantArray[0, (n - 3)/2]}];
	  AppendTo[b, Join[ConstantArray[0, (n - 3)/2], {1}]];
	  
	  bottom = q;
	  For[row = 2, row <= (m - 1)/2, row++,
	   bottom = Join[bottom, q, 2];
	   ];
	  bottom = Join[bottom, b, 2];
	  
	  mat = {};
	  For[row = 1, row <= (m - 1)/2 - 1, row++,
	   r = Join[
	     ConstantArray[0, n*(row - 1)],
	     Range[n],
	     Reverse[Range[n - 1]],
	     ConstantArray[0, (m*n - 1)/2 - n*(row - 1) - (2 n - 1)]
	     ];
	   AppendTo[mat, r];
	   ];
	  
	  AppendTo[mat, 
	   Join[ConstantArray[0, (m*n - 1)/2 - n - (n - 1)/2], 
	    Range[2, 2 n - 2, 2], ConstantArray[2 n, (n - 1)/2], {n}]];
	  mat = Join[mat, bottom];
	  mat = Permute[mat,Cycles[{{Length[mat],Length[mat]-1}}]];
	  
	  Return[mat]
	  ] /; s>=3 && p==5*s+2

(* ::Subsection:: *)
(* C_n *)
ProjectionMatrix[origin:Algebra[C][3] ,ProductAlgebra[Algebra[A][1],Algebra[A][1]]] := {{2, 4, 4},{1, 0, 1}}
ProjectionMatrix[origin:Algebra[C][5] ,ProductAlgebra[Algebra[A][1],Algebra[C][2]]] := SpecialSubalgebra[origin, {ProductIrrep[Irrep[A][1],Irrep[C][0,1]]}]
ProjectionMatrix[origin:Algebra[C][6] ,ProductAlgebra[Algebra[A][1],Algebra[C][2]]] := SpecialSubalgebra[origin, {ProductIrrep[Irrep[A][2],Irrep[C][1,0]]}]
ProjectionMatrix[origin:Algebra[C][8] ,ProductAlgebra[Algebra[C][2]]] := SpecialSubalgebra[origin, {ProductIrrep[Irrep[C][1,1]]}]
ProjectionMatrix[origin:Algebra[C][7] ,ProductAlgebra[Algebra[A][1],Algebra[B][3]]] := {{1,2,3,4,5,6,7},{1,0,0,0,0,1,0},{0,1,0,0,1,0,0},{0,0,2,2,0,0,0}}
ProjectionMatrix[origin:Algebra[C][7] ,ProductAlgebra[Algebra[C][3]]] := {{0,0,1,0,2,2,4},{0,2,2,3,1,0,0},{1,0,0,0,1,2,1}}
ProjectionMatrix[origin:Algebra[C][9] ,ProductAlgebra[Algebra[A][1],Algebra[B][4]]] := {{1,2,3,4,5,6,7,8,9},{1,0,0,0,0,0,0,1,0},{0,1,0,0,0,0,1,0,0},{0,0,1,0,0,1,0,0,0},{0,0,0,2,2,0,0,0,0}}
ProjectionMatrix[origin:Algebra[C][10] ,ProductAlgebra[Algebra[A][5]]] := {{0,0,1,1,0,1,1,0,1,2},{0,1,0,1,1,0,1,1,1,0},{1,0,0,0,0,1,1,2,1,2},{0,1,2,1,2,1,1,0,0,0},{0,0,0,1,1,2,1,2,3,2}}
ProjectionMatrix[origin:Algebra[C][15] ,ProductAlgebra[Algebra[C][2],Algebra[C][3]]] := Permute[{{1,2,3,4,5,6,5,4,3,2,1,0,0,0,0},{0,0,0,0,0,0,2,4,6,8,10,12,12,12,12},{1,0,0,0,1,0,1,0,0,0,1,0,1,0,0},{0,1,0,1,0,0,0,1,0,1,0,0,0,1,0},{0,0,1,0,0,0,0,0,1,0,0,0,0,0,1}},Cycles[{{1,2}}]]
ProjectionMatrix[origin:Algebra[C][m_] ,ProductAlgebra[Algebra[D][n_],Algebra[A][1]]] := 
	Module[{mat, rightSide},
		mat = IdentityMatrix[n - 1];
	  	AppendTo[mat, ConstantArray[0, n - 1]];
	  	mat = Join[mat, IdentityMatrix[n - 1]];
	  	AppendTo[mat, ConstantArray[0, n - 1]];
	  
	  	rightSide = ConstantArray[0, n - 2];
	  	rightSide = Join[rightSide, {1}, ConstantArray[2, n - 1], {3,4}];
	  	mat = MapThread[Append, {mat, rightSide}];
	  
	  	rightSide = Join[Range[n], Reverse[Range[n - 1]], {0}];
	  	mat = MapThread[Append, {mat, rightSide}];
	  
	  	Return[Transpose[mat]]
	] /; m == 2*n && n >= 4
ProjectionMatrix[origin : Algebra[C][m_], ProductAlgebra[Algebra[B][n_], Algebra[A][1]]] := 
	Module[{mat},
		mat = IdentityMatrix[n - 1];
		mat = Join[mat, ConstantArray[0, {2, n - 1}]];
		mat = Join[mat, Reverse[IdentityMatrix[n - 1]]];
		AppendTo[mat, ConstantArray[0, n - 1]];
		mat = MapThread[Append, {mat, Join[ConstantArray[0, n - 1], {2, 2}, ConstantArray[0, n]]}];
		mat = MapThread[Append, {mat, Range[2 n + 1]}];
		Return[Transpose[mat]];
	] /; m == 2*n + 1 && n >= 3
ProjectionMatrix[origin:Algebra[C][4] ,ProductAlgebra[Algebra[A][1],Algebra[A][1],Algebra[A][1]]] := {{1,2,3,4},{1,0,1,0},{1,2,1,0}}
ProjectionMatrix[origin:Algebra[C][6] ,ProductAlgebra[Algebra[A][1],Algebra[A][3]]] := {{1,2,3,4,5,6},{0,1,0,1,0,0},{1,0,0,0,1,0},{0,1,2,1,0,0}}
ProjectionMatrix[origin:Algebra[C][n_] ,ProductAlgebra[Algebra[A][1]]] := SpecialSubalgebra[origin,{ProductIrrep[Irrep[Algebra[A][1]][2*n]]}] /; n>=2
(*untestable but runs*) ProjectionMatrix[origin:Algebra[C][t_] ,ProductAlgebra[Algebra[C][m_],Algebra[D][s_]]] := 
	Module[{mid, bottom, f, d, e, row, r, mat,n,i}, 
	  n=2*s;
	  
	  f = IdentityMatrix[(n - 2)/2];
	  f = MapThread[Append, {f, ConstantArray[0, (n - 2)/2]}];
	  f = Join[f, Reverse[IdentityMatrix[(n - 2)/2]], 2];
	  AppendTo[f, 
	   Join[ConstantArray[0, (n - 2)/2], {121}, 
	    ConstantArray[0, (n - 2)/2]]];
	  
	  d = {};
	  For[row = 1, row <= m - 2, row++, r = Join[
	     ConstantArray[0, n*(row - 1)],
	     Range[n],
	     Reverse[Range[n - 1]],
	     ConstantArray[0, m*n - n*(row - 1) - (2 n - 1)]
	     ];
	   AppendTo[d, r];
	   ];
	  
	  e = {
	    Join[Range[n - 1], Reverse[Range[0, n]]],
	    Join[ConstantArray[0, n], Range[n]]
	    };
	  
	  mid = Join[ConstantArray[0, {2, m*n - 2 n}], e, 2];
	  bottom = MapThread[Append, {f, ConstantArray[0, n/2]}];
	  For[i = 2, i <= m, i++, 
	   bottom = 
	    Join[bottom, MapThread[Append, {f, ConstantArray[0, n/2]}], 2]
	   ];
	  mat = Join[d, mid, bottom];
	  Return[mat]
	  ] /; m >= s && s >= 2 && m>=3 && t==2*s*m
ProjectionMatrix[origin:Algebra[C][t_] ,ProductAlgebra[Algebra[D][s_],Algebra[C][n_]]]:= 
	Module[{row, r, d, l, p, mid, bottom, mat,m},
	   m=2*s;
	   
	   d = {};
	   For[row = 1, row <= (m/2 - 2), row++,
	    r = Join[
	      ConstantArray[0, 2 n*(row - 1)],
	      Range[2 n],
	      Reverse[Range[2 n - 1]],
	      ConstantArray[0, m*n - 2 n*(row - 1) - (4 n - 1)]
	      ];
	    AppendTo[d, r];
	    ];
	   
	   l = {
	     Join[Range[0, 2 n], Reverse[Range[0, 2 n - 1]]],
	     Range[0, 4 n]
	     };
	   
	   p = MapThread[
	     Append, {IdentityMatrix[n - 1], ConstantArray[0, n - 1]}];
	   p = Join[p, Reverse[IdentityMatrix[n - 1]], 2];
	   AppendTo[p, 
	    Join[ConstantArray[0, n - 1], {1}, ConstantArray[0, n - 1]]];
	   p = MapThread[Append, {p, ConstantArray[0, n]}];
	   
	   mid = Join[ConstantArray[0, {2, m*n - 4 n - 1}], l, 2];
	   bottom = p;
	   For[row = 2, row <= m/2, row++,
	    bottom = Join[bottom, p, 2]
	    ];
	   
	   mat = Join[d, mid, bottom];
	   Return[mat]
	   ] /; n >= 2 && s >= n && s>=3 &&t==2*s*n
ProjectionMatrix[origin:Algebra[C][12] ,ProductAlgebra[Algebra[A][3],Algebra[C][2]]]:={{0,0,0,0,1,2,3,4,3,2,1,0},{1,2,3,4,3,2,1,0,0,0,0,0},{0,0,0,0,1,2,3,4,5,6,7,8},{1,0,1,0,1,0,1,0,1,0,1,0},{0,1,0,0,0,1,0,0,0,1,0,0}}
(*untestable*) ProjectionMatrix[origin:Algebra[C][t_] ,ProductAlgebra[Algebra[C][m_],Algebra[B][k_]]]:= 
	Module[{row, r, d, s, q, mid, bottom, mat,n},
	  n = 2*k+1;
	  
	  d = {};
	  For[row = 1, row <= (m - 2), row++,
	   r = Join[
	     ConstantArray[0, n*(row - 1)],
	     Range[n],
	     Reverse[Range[n - 1]],
	     ConstantArray[0, m*n - n*(row - 1) - (2 n - 1)]
	     ];
	   AppendTo[d, r];
	   ];
	  
	  s = {
	    Join[Range[0, n], Reverse[Range[0, n - 1]]],
	    Join[ConstantArray[0, n + 1], Range[n]]
	    };
	  
	  q = Join[
	    IdentityMatrix[(n - 3)/2],
	    ConstantArray[0, {(n - 3)/2, 2}],
	    Reverse[IdentityMatrix[(n - 3)/2]], 2
	    ];
	  AppendTo[q, 
	   Join[ConstantArray[0, (n - 3)/2], {2, 2}, 
	    ConstantArray[0, (n - 3)/2]]];
	  q = MapThread[Append, {q, ConstantArray[0, (n - 1)/2]}];
	  
	  mid = Join[ConstantArray[0, {2, m*n - 2 n - 1}], s, 2];
	  bottom = q;
	  For[row = 2, row <= m, row++,
	   bottom = Join[bottom, q, 2];
	   ];
	  
	  mat = Join[d, mid, bottom];
	  Return[mat]
	  ] /; m >= k && k >= 2 && t == m*(2*k+1)
ProjectionMatrix[origin:Algebra[C][10] ,ProductAlgebra[Algebra[C][2],Algebra[C][2]]]:= {{1,2,3,4,5,4,3,2,1,0},{0,0,0,0,0,1,2,3,4,5},{0,2,2,0,0,0,2,2,0,0},{1,0,0,1,0,1,0,0,1,0}}
(*works for B3xC2. untestable otherwise...*) ProjectionMatrix[origin:Algebra[C][k_] ,ProductAlgebra[Algebra[B][s_],Algebra[C][n_]]]:= 
	Module[{row, r, d, bottom, p, t, mat,m},
	  m=2*s+1;
	  
	  d = {};
	  For[row = 1, row <= (m - 3)/2, row++,
	   r = Join[
	     ConstantArray[0, 2 n*(row - 1)],
	     Range[2 n],
	     Reverse[Range[2 n - 1]],
	     ConstantArray[0, m*n - 2 n*(row - 1) - (4 n - 1)]
	     ];
	   AppendTo[d, r];
	   ];
	  
	  p = MapThread[
	    Append, {IdentityMatrix[n - 1], ConstantArray[0, n - 1]}];
	  p = Join[p, Reverse[IdentityMatrix[n - 1]], 2];
	  AppendTo[p, 
	   Join[ConstantArray[0, n - 1], {1}, ConstantArray[0, n - 1]]];
	  p = MapThread[Append, {p, ConstantArray[0, n]}];
	  
	  t = MapThread[
	    Append, {IdentityMatrix[n - 1], ConstantArray[0, n - 1]}];
	  AppendTo[t, Join[ConstantArray[0, n - 1], {1}]];
	  
	  bottom = p;
	  For[row = 2, row <= (m - 1)/2, row++,
	   bottom = Join[bottom, p, 2];
	   ];
	  bottom = Join[bottom, t, 2];
	  
	  mat = d;
	  AppendTo[mat, 
	   Join[ConstantArray[0, m*n - 3 n], Range[2, 4 n, 2], 
	    ConstantArray[4 n, n]]];
	  mat = Join[mat, bottom];
	  
	  Return[mat];
	  ] /; s >= n && n > 1 && k==n*(2*s+1)
ProjectionMatrix[origin:Algebra[C][n_],ProductAlgebra[Algebra[A][1],Algebra[C][m_]]] 				 			 :=
	Module[{top, bottom, mat},
		 top = {Join[Range[2, 4*m, 2], ConstantArray[4*m, m]]};
		 bottom = Join[IdentityMatrix[m], Join[Reverse@IdentityMatrix[m - 1], {ConstantArray[0, m - 1]}], 2];
		 bottom = MapThread[Append, {bottom, ConstantArray[0, m]}];
		 bottom = Join[bottom, IdentityMatrix[m], 2];
		 mat = Join[top, bottom];
		 mat
	 ] /; n==3*m && m>=3
(*UNTESTED*) ProjectionMatrix[origin:Algebra[C][28],ProductAlgebra[E7]] := SpecialSubalgebra[origin, {ProductIrrep[Irrep[E7][0,0,0,0,0,1,0]]}]


(* ::Subsection:: *)
(* D_n *)
ProjectionMatrix[origin:Algebra[D][4] ,ProductAlgebra[Algebra[A][2]]] :=  SpecialSubalgebra[origin, {ProductIrrep[Irrep[A][1,1]]}]
(* ProjectionMatrix[origin:Algebra[D][4] ,ProductAlgebra[Algebra[B][3]]] :=  {{0,0,1,0},{0,1,0,0},{1,0,0,1}} *)
ProjectionMatrix[origin:Algebra[D][4] ,ProductAlgebra[Algebra[B][3]]] :=  SpecialSubalgebra[origin, {ProductIrrep[Irrep[B][0,0,0]],ProductIrrep[Irrep[B][1,0,0]]}]
ProjectionMatrix[origin:Algebra[D][5] ,ProductAlgebra[Algebra[C][2]]] :=  {{2,2,4,1,1},{0,1,0,1,1}}
ProjectionMatrix[origin:Algebra[D][5] ,ProductAlgebra[Algebra[A][1],Algebra[B][3]]] :=  SpecialSubalgebra[origin, {ProductIrrep[Irrep[A][1],Irrep[B][0,0,1]]}]
ProjectionMatrix[origin:Algebra[D][5] ,ProductAlgebra[Algebra[B][4]]] :=  SpecialSubalgebra[origin, {ProductIrrep[Irrep[B][0,0,0,1]]}]
ProjectionMatrix[origin:Algebra[D][7] ,ProductAlgebra[Algebra[C][2]]] :=  {{0,2,6,6,8,3,3},{2,2,0,1,0,1,1}}
ProjectionMatrix[origin:Algebra[D][7] ,ProductAlgebra[Algebra[C][3]]] :=  {{0,1,0,1,0,1,1},{1,0,0,1,3,1,1},{0,1,2,1,0,0,0}}
ProjectionMatrix[origin:Algebra[D][7] ,ProductAlgebra[G2]] := Permute[{{1,0,0,1,0,1,1},{0,3,4,3,5,1,1}},Cycles[{{1,2}}]]
ProjectionMatrix[origin:Algebra[D][8] ,ProductAlgebra[Algebra[B][4]]] :=  {{0,0,0,1,1,0,1,0},{0,0,1,0,1,1,0,0},{0,1,0,0,0,0,0,1},{1,0,1,2,1,2,1,0}}
ProjectionMatrix[origin:Algebra[D][9] ,ProductAlgebra[Algebra[A][1],Algebra[A][3]]] := {{2,4,6,8,10,12,12,6,6},{0,1,0,1,0,0,0,1,0},{1,0,0,0,1,0,1,0,0},{0,1,2,1,0,0,0,0,1}}
ProjectionMatrix[origin:Algebra[D][10] ,ProductAlgebra[Algebra[A][3]]] := {{0,1,0,2,3,3,2,4,1,1},{2,2,3,1,2,1,3,2,1,1},{0,1,2,4,3,5,4,4,3,3}}
ProjectionMatrix[origin:Algebra[D][12] ,ProductAlgebra[Algebra[C][3],Algebra[C][2]]] := {{1,2,3,4,3,2,1,0,0,0,0,0},{0,0,0,0,1,2,3,4,3,2,1,0},{0,0,0,0,0,0,0,0,1,2,1,2},{1,0,1,0,1,0,1,0,1,0,1,0},{0,1,0,0,0,1,0,0,0,1,0,0}}
ProjectionMatrix[origin:Algebra[D][12] ,ProductAlgebra[Algebra[A][1],Algebra[D][4]]] := {{2,4,6,8,10,12,14,16,16,16,8,8},{1,0,0,0,0,0,1,0,1,0,0,0},{0,1,0,0,0,1,0,0,0,1,0,0},{0,0,1,0,1,0,0,0,0,0,1,0},{0,0,1,2,1,0,0,0,0,0,0,1}}
ProjectionMatrix[origin:Algebra[D][12] ,ProductAlgebra[Algebra[A][4]]] := {{1,2,1,0,0,1,3,2,2,2,1,1},{0,0,1,2,1,2,1,3,2,2,1,1},{0,1,1,2,3,2,2,1,3,2,1,1},{1,0,1,0,1,1,1,1,0,2,1,1}}
ProjectionMatrix[origin:Algebra[D][13] ,ProductAlgebra[F4]] := {{0,0,0,1,0,1,0,1,1,0,0,0,0},{0,0,1,0,0,0,1,1,0,1,1,0,0},{0,1,0,1,2,1,0,0,1,1,0,1,1},{1,0,0,0,0,1,2,1,2,1,3,1,1}}
ProjectionMatrix[origin:Algebra[D][14] ,ProductAlgebra[Algebra[D][4]]] := {{0,1,0,1,2,1,0,1,1,1,3,2,1,1},{1,0,0,0,0,1,2,3,2,1,0,2,1,1},{0,1,2,1,2,1,2,1,1,3,3,2,1,1},{0,1,2,3,2,3,2,1,3,3,3,2,1,1}}
ProjectionMatrix[origin:Algebra[D][15] ,ProductAlgebra[Algebra[A][1],Algebra[D][5]]] := {{2,4,6,8,10,12,14,16,18,20,20,20,20,10,10},{1,0,0,0,0,0,0,0,1,0,1,0,0,0,0},{0,1,0,0,0,0,0,1,0,0,0,1,0,0,0},{0,0,1,0,0,0,1,0,0,0,0,0,1,0,0},{0,0,0,1,0,1,0,0,0,0,0,0,0,1,0},{0,0,0,1,2,1,0,0,0,0,0,0,0,0,1}}
ProjectionMatrix[origin:Algebra[D][15] ,ProductAlgebra[Algebra[C][2],Algebra[A][3]]] := {{0,0,0,0,0,0,2,4,6,8,10,12,12,6,6},{1,2,3,4,5,6,5,4,3,2,1,0,0,0,0},{0,1,0,1,0,0,0,1,0,1,0,0,0,1,0},{1,0,0,0,1,0,1,0,0,0,1,0,1,0,0},{0,1,2,1,0,0,0,1,2,1,0,0,0,0,1}}
ProjectionMatrix[origin:Algebra[D][16] ,ProductAlgebra[Algebra[A][1],Algebra[A][1],Algebra[D][4]]] := {{1,2,3,4,5,6,7,8,7,6,5,4,3,2,1,0},{1,2,3,4,5,6,7,8,9,10,11,12,13,14,7,8},{1,0,0,0,0,0,1,0,1,0,0,0,0,0,1,0},{0,1,0,0,0,1,0,0,0,1,0,0,0,1,0,0},{0,0,1,0,1,0,0,0,0,0,1,0,1,0,0,0},{0,0,1,2,1,0,0,0,0,0,1,2,1,0,0,0}}
ProjectionMatrix[origin:Algebra[D][n_] ,ProductAlgebra[Algebra[B][m_]]] := 
	Module[{mat, rightSide},
	   If[n == 2, mat = {}, mat = IdentityMatrix[n - 2]];
	   AppendTo[mat, ConstantArray[0, n - 2]];
	   AppendTo[mat, ConstantArray[0, n - 2]];
	   
	   rightSide = ConstantArray[0, n - 2];
	   rightSide = Join[rightSide, {1, 1}];
	   mat = MapThread[Append, {mat, rightSide}];
	   
	   Return[Transpose[mat]]
	   ] /; (n >= 3 && m == n - 1)
ProjectionMatrix[origin:Algebra[D][n_] ,ProductAlgebra[Algebra[B][r_],Algebra[B][s_]]] := 
	Module[{mat, mid, right},
	   If[r == 1, mat = {}, mat = IdentityMatrix[r - 1]];
	   mat = Join[mat, ConstantArray[0, {n - r + 1, r - 1}]];
	
	   mid = Join[ConstantArray[0, r - 1], ConstantArray[2, n - r - 1], {1,1}];
	   mat = MapThread[Append, {mat, mid}];
	   
	   If[n - r - 1 != 0,
	    right = ConstantArray[0, {r, n - r - 1}];
	    right = Join[right, IdentityMatrix[n - r - 1]];
	    AppendTo[right, Join[ConstantArray[0, n - r - 2], {1}]];
	    mat = Join[mat, right, 2];
	    ];
	   
	   Return[Transpose[mat]];
	   ] /; n >= 7 && r > 2 && s>2 && r <= n - 2 && s==n-r-1
ProjectionMatrix[origin:Algebra[D][m_] ,ProductAlgebra[Algebra[C][n_],Algebra[A][1]]] := 
	Module[{mat, right},
	   mat = IdentityMatrix[n];
	   If[n > 1,
	    mat = 
	      Join[mat, 
	       MapThread[
	        Append, {Reverse[IdentityMatrix[n - 1]], 
	         ConstantArray[0, n - 1]}]];
	    ];
	   AppendTo[mat, ConstantArray[0, n]];
	   
	   right = Join[Range[n], Range[n + 1, 2 n - 2]];
	   If[n > 1, right = Join[right, {n - 1}]];
	   right = Join[right, {n}];
	   mat = MapThread[Append, {mat, right}];
	   
	   Return[Transpose[mat]]
	   ] /; n >= 2 && m == 2*n
ProjectionMatrix[origin:Algebra[D][n_] ,ProductAlgebra[Algebra[A][1],Algebra[B][m_]]] := 
	Module[{mat, mid},
	   mat = Join[ConstantArray[2, n - 2], {1, 1}];
	   
	   mid = Transpose[
	     Join[{ConstantArray[0, n - 3]}, 
	      IdentityMatrix[n - 3], {ConstantArray[0, n - 3]}]];
	   mid = MapThread[Append, {mid, ConstantArray[0, n - 3]}];
	   mat = Join[{mat}, mid];
	   
	   AppendTo[mat, Join[ConstantArray[0, n - 2], {1, 1}]];
	   
	   Return[mat];
	   ] /; n >= 5 && m==n-2
ProjectionMatrix[origin:Algebra[D][n_] ,ProductAlgebra[Algebra[C][2],Algebra[B][m_]]] := 
	Module[{mat, mid},
	   mid = Join[ConstantArray[0, {n - 4, 3}], IdentityMatrix[n - 4], 2];
	   mid = MapThread[Append, {mid, ConstantArray[0, n - 4]}];
	   
	   mat = Join[
	     {Join[{0}, ConstantArray[2, n - 3], {1, 1}]},
	     {Join[{1}, ConstantArray[0, n - 1]]},
	     mid,
	     {Join[ConstantArray[0, n - 2], {-1, 1}]}
	     ];
	   
	   Return[mat]
	   ] /; n >= 6 && m==n-3
ProjectionMatrix[origin:Algebra[D][5] ,ProductAlgebra[Algebra[C][2],Algebra[C][2]]] := {{0,2,2,1,1},{1,0,0,0,0},{0,0,0,-1,1},{0,0,0,1,0}}
ProjectionMatrix[origin:Algebra[D][4] ,ProductAlgebra[Algebra[C][2],Algebra[A][1]]] := SpecialSubalgebra[
 Algebra[D][4],
 {ProductIrrep[Irrep[C][0, 0], Irrep[A][2]], 
  ProductIrrep[Irrep[C][0, 1], Irrep[A][0]]}
 ]
(*correct for sp6xsp4,anything larger is too much memory*) ProjectionMatrix[origin:Algebra[D][s_] ,ProductAlgebra[Algebra[C][m_],Algebra[C][n_]]] := 
	Module[{row, r, d, j, k, mid, bottom, mat},
	  d = {};
	  For[row = 1, row <= (m - 2), row++,
	   r = Join[
	     ConstantArray[0, 2 n*(row - 1)],
	     Range[2 n],
	     Reverse[Range[2 n - 1]],
	     ConstantArray[0, 2 m*n - 2 n*(row - 1) - (4 n - 1)]
	     ];
	   AppendTo[d, r];
	   ];
	  
	  j = {
	    Join[Range[0, 2 n], Reverse[Range[2 n - 1]], {0}],
	    Join[ConstantArray[0, 2 n + 1], Range[2 n - 2], {n - 1, n}]
	    };
	  
	  k = MapThread[
	    Append, {IdentityMatrix[n - 1], ConstantArray[0, n - 1]}];
	  k = Join[k, Reverse[IdentityMatrix[n - 1]], 2];
	  AppendTo[k, 
	   Join[ConstantArray[0, n - 1], {1}, ConstantArray[0, n - 1]]];
	  k = MapThread[Append, {k, ConstantArray[0, n]}];
	  
	  mid = Join[ConstantArray[0, {2, 2 m*n - 4 n - 1}], j, 2];
	  
	  bottom = k;
	  For[row = 2, row <= m, row++,
	   bottom = Join[bottom, k, 2];
	   ];
	  
	  mat = Join[d, mid, bottom];
	  Return[mat]
	  ] /; m >= n && m>=3 && EvenQ[m] && EvenQ[n] && n >= 2 && s==2*m*n
ProjectionMatrix[origin:Algebra[D][8] ,ProductAlgebra[Algebra[C][2],Algebra[C][2]]] := {{1,2,3,4,3,2,1,0},{0,0,0,0,1,2,1,2},{1,0,1,0,1,0,1,0},{0,1,0,0,0,1,0,0}}
(*untestable*) ProjectionMatrix[origin:Algebra[D][k_] ,ProductAlgebra[Algebra[B][s_],Algebra[D][t_]]] := 
	Module[{r, row, g, bottom, mat,n,m},
	  n=2*t;
	  m=2*s+1;
	  
	  mat = {};
	  For[row = 1, row <= (m - 1)/2 - 1, row++,
	   r = Join[
	     ConstantArray[0, n*(row - 1)],
	     Range[n],
	     Reverse[Range[n - 1]],
	     ConstantArray[0, m*n/2 - n*(row - 1) - (2 n - 1)]
	     ];
	   AppendTo[mat, r];
	   ];
	  
	  AppendTo[mat,
	   Join[
	    ConstantArray[0, m*n/2 - n - n/2],
	    Range[2, 2 (n - 1), 2],
	    ConstantArray[2 n, n/2 - 1],
	    {n, n}
	    ]
	   ];
	  
	  g = Join[IdentityMatrix[(n - 4)/2], 
	    ConstantArray[0, {(n - 4)/2, 3}], 
	    Reverse[IdentityMatrix[(n - 4)/2]], 2];
	  AppendTo[g, 
	   Join[ConstantArray[0, (n - 4)/2], {1, 0, 1}, 
	    ConstantArray[0, (n - 4)/2]]];
	  AppendTo[g, 
	   Join[ConstantArray[0, (n - 4)/2], {1, 2, 1}, 
	    ConstantArray[0, (n - 4)/2]]];
	  g = MapThread[Append, {g, ConstantArray[0, n/2]}];
	  
	  bottom = g;
	  For[row = 2, row <= (m - 1)/2, row++,
	   bottom = Join[bottom, g, 2];
	   ];
	  bottom = Join[bottom, IdentityMatrix[n/2], 2];
	  
	  mat = Join[mat, bottom];
	  Return[mat]
	  ] /; s >= t && t > 3 && k==t*(2*s+1)
(*untestable*) ProjectionMatrix[origin:Algebra[D][k_] ,ProductAlgebra[Algebra[D][s_],Algebra[B][t_]]] := 
	Module[{r, row, d, q, mid, bottom, mat,n,m},
	  m=2*s;
	  n=2*t+1;
	  
	  d = {};
	  For[row = 1, row <= m/2 - 2, row++,
	   r = Join[
	     ConstantArray[0, n*(row - 1)],
	     Range[n],
	     Reverse[Range[n - 1]],
	     ConstantArray[0, m*n/2 - n*(row - 1) - (2 n - 1)]
	     ];
	   AppendTo[d, r];
	   ];
	  
	  q = Join[
	    IdentityMatrix[(n - 3)/2],
	    ConstantArray[0, {(n - 3)/2, 2}],
	    Reverse[IdentityMatrix[(n - 3)/2]], 2
	    ];
	  AppendTo[q, 
	   Join[ConstantArray[0, (n - 3)/2], {2, 2}, 
	    ConstantArray[0, (n - 3)/2]]];
	  q = MapThread[Append, {q, ConstantArray[0, (n - 1)/2]}];
	  
	  mid = {
	    Join[Range[n], Reverse[Range[0, n - 1]]],
	    Join[Range[2 n - 2], {n - 1, n}]
	    };
	  mid = Join[ConstantArray[0, {2, m*n/2 - 2 n}], mid, 2];
	  
	  bottom = q;
	  For[row = 2, row <= m/2, row++,
	   bottom = Join[bottom, q, 2];
	   ];
	  
	  mat = Join[d, mid, bottom];
	  Return[mat]
	  ] /; t >= 2 && s >= t && s>3 && k==s*(2*t+1)
(*untestable*) ProjectionMatrix[origin:Algebra[D][k_] ,ProductAlgebra[Algebra[D][s_],Algebra[D][t_]]] := 
	Module[{row, r, d, g, h, m, n, mid, bottom, mat},
	  d = {};
	  For[row = 1, row <= (m/2 - 2), row++,
	   r = Join[
	     ConstantArray[0, n*(row - 1)],
	     Range[n],
	     Reverse[Range[n - 1]],
	     ConstantArray[0, m*n/2 - n - n*row + 1]
	     ];
	   AppendTo[d, r];
	   ];
	  
	  g = Join[IdentityMatrix[(n - 4)/2], 
	    ConstantArray[0, {(n - 4)/2, 3}], 
	    Reverse[IdentityMatrix[(n - 4)/2]], 2];
	  AppendTo[g, 
	   Join[ConstantArray[0, (n - 4)/2], {1, 0, 1}, 
	    ConstantArray[0, (n - 4)/2]]];
	  AppendTo[g, 
	   Join[ConstantArray[0, (n - 4)/2], {1, 2, 1}, 
	    ConstantArray[0, (n - 4)/2]]];
	  g = MapThread[Append, {g, ConstantArray[0, n/2]}];
	  
	  h = {
	    Join[Range[0, n], Reverse[Range[0, n - 1]]],
	    Join[Range[0, n], Range[n + 1, 2 n - 2], {n - 1, n}]
	    };
	  
	  mid = Join[ConstantArray[0, {2, m*n/2 - 2 n - 1}], h, 2];
	  bottom = g;
	  For[i = 2, i <= m/2, i++,
	   bottom = Join[bottom, g, 2];
	   ];
	  mat = Join[d, mid, bottom];
	  
	  Return[mat]
	  ] /; s >= t && t >= 4 && k==2*s*t


(* ::Subsection:: *)
(* E_6 *)
ProjectionMatrix[origin:E6, ProductAlgebra[Algebra[A][2]]] := {{2,2,5,2,2,1},{2,5,5,5,2,4}}
ProjectionMatrix[origin:E6, ProductAlgebra[G2]] := Permute[{{0,1,0,1,0,1},{2,2,5,2,2,1}},Cycles[{{1,2}}]]
(*Kim et al version*) ProjectionMatrix[origin:E6, ProductAlgebra[Algebra[C][4]]] := {{0,1,0,1,0,0},{1,0,0,0,1,0},{0,1,2,1,0,0},{0,0,0,0,0,1}}
(*Kim et al version*) ProjectionMatrix[origin:E6, ProductAlgebra[F4]] := {{0,0,0,0,0,1},{0,0,1,0,0,0},{0,1,0,1,0,0},{1,0,0,0,1,0}}
ProjectionMatrix[origin:E6, ProductAlgebra[Algebra[A][2], G2]] := Permute[{{1,2,1,0,0,1},{0,0,1,2,1,1},{0,1,1,1,0,0},{1,0,1,0,1,1}},Cycles[{{3,4}}]] (*tried {{1,2},{3,4}} <- off for conjugates, {{3,4}}, {{1,2}}*)


(* ::Subsection:: *)
(* E_7 *)
ProjectionMatrix[origin:E7, ProductAlgebra[Algebra[A][1]]] := {{34,66,96,75,52,27,49}}
(**)
(*Multiple E7 -> SU(2) branchings:*) 
(*1*) ProjectionMatrix[origin:E7, ProductAlgebra[Algebra[A][1]],1] := {{34,66,96,75,52,27,49}}
(*2*) ProjectionMatrix[origin:E7, ProductAlgebra[Algebra[A][1]],2] := {{26,50,72,57,40,21,37}}
(**)
(*Kim et al. version*) ProjectionMatrix[origin:E7, ProductAlgebra[Algebra[A][2]]] := {{4,6,11,7,6,0,4},{4,9,11,10,6,6,7}}
(*Kim et al. version*) ProjectionMatrix[origin:E7, ProductAlgebra[Algebra[A][1], Algebra[A][1]]] := {{8,16,24,18,12,6,12},{2,2,0,3,4,3,1}}
ProjectionMatrix[origin:E7, ProductAlgebra[Algebra[A][1], G2]] := Permute[{{2,4,4,5,4,1,3},{0,1,1,0,1,1,1},{2,2,4,4,1,0,1}},Cycles[{{2,3}}]]
(*Kim et al. version*) ProjectionMatrix[origin:E7, ProductAlgebra[F4, Algebra[A][1]]] := {{0,0,0,0,0,0,1},{0,0,1,0,0,0,0},{0,1,0,1,0,0,0},{1,0,0,0,1,0,0},{2,4,6,5,4,3,3}}
ProjectionMatrix[origin:E7, ProductAlgebra[G2, Algebra[C][3]]] := Permute[{{0,0,1,1,0,0,1},{1,2,1,1,2,1,0},{0,1,1,1,0,1,0},{1,0,1,1,1,0,0},{0,1,1,0,0,0,1}},Cycles[{{1,2}}]]


(* ::Subsection:: *)
(* E_8 *)
ProjectionMatrix[origin:E8, ProductAlgebra[Algebra[A][1]]] := {{72,142,210,172,132,90,46,106}}
(**)
(*Multiple E8 -> SU(2) branchings:*)
(*1*) ProjectionMatrix[origin:E8, ProductAlgebra[Algebra[A][1]],1] := {{72,142,210,172,132,90,46,106}}
(*2*) ProjectionMatrix[origin:E8, ProductAlgebra[Algebra[A][1]],2] := {{60,118,174,142,108,74,38,88}}
(*3*) ProjectionMatrix[origin:E8, ProductAlgebra[Algebra[A][1]],3] := {{92,182,270,220,168,114,58,136}}
(**)
ProjectionMatrix[origin:E8, ProductAlgebra[G2, F4]] := Permute[{{0,0,1,1,0,0,0,1},{1,2,1,1,2,1,1,0},{0,0,0,1,1,1,0,0},{0,1,1,0,0,0,0,1},{1,0,1,1,1,0,0,0},{0,1,1,1,0,1,1,0}},Cycles[{{1,2}}]]
(*Kim et al. version*) ProjectionMatrix[origin:E8, ProductAlgebra[Algebra[A][1], Algebra[A][2]]] := {{8,16,22,16,14,10,6,12},{2,4,8,6,4,4,1,3},{2,4,5,6,4,1,1,3}}
ProjectionMatrix[origin:E8, ProductAlgebra[Algebra[C][2]]] := {{4,8,16,12,8,8,2,6},{4,8,9,8,7,3,3,6}}


(* ::Subsection:: *)
(* G_2 *)
ProjectionMatrix[origin:G2,ProductAlgebra[Algebra[A][1]]] := SpecialSubalgebra[origin, {ProductIrrep[Irrep[A][6]]}]


(* ::Subsection:: *)
(* F_4 *)
ProjectionMatrix[origin:F4, ProductAlgebra[Algebra[A][1]]] := {{22,42,30,16}}
ProjectionMatrix[origin:F4, ProductAlgebra[Algebra[A][1],G2]] := Permute[{{4,8,6,4},{0,1,0,0},{1,0,1,0}},Cycles[{{2,3}}]]


End[]
EndPackage[]
