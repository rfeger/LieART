(* ::Package:: *)
(* :Title: LieART *)
(* :Authors: Robert Feger, Robert Saskowski *)
(* :Summary: *)
(* :Context:   LieART` *)
(* :Package version:   2.1.1 *)
(* :Copyright:  Copyright 2012-2025, Robert Feger, Thomas Kephart and Robert Saskowski.

This program is free software: you can redistribute it and/or modify
it under the terms of the GNU Lesser General Public License as published by
the Free Software Foundation, either version 3 of the License, or
(at your option) any later version.

This program is distributed in the hope that it will be useful,
but WITHOUT ANY WARRANTY; without even the implied warranty of
MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
GNU Lesser General Public License for more details.

You should have received a copy of the GNU Lesser General Public License
along with this program.  If not, see <http://www.gnu.org/licenses/>.
*)

(*  :Keywords:  Lie algebras, Lie groups, irreps, representations  *)

(*  :Mathematica version: 14.2   *)

(*  :Discussion: *)

Print["LieART 2.1.1"];
Print["last revised 14 February 2025"]

BeginPackage["LieART`LieART`"]
Get["LieART`BranchingRules`"]

(* Algebras *)
CartanMatrix::usage = "CartanMatrix[algebra] is the Cartan matrix of algebra."
MetricTensor::usage = "MetricTensor[algebra] is the metric tensor of algebra."

A::usage  = "A represents the infinite A series Lie algebras." 
B::usage  = "B represents the infinite B series Lie algebras." 
C::usage  = "C represents the infinite C series Lie algebras." 
D::usage  = "D represents the infinite D series Lie algebras." 
U::usage  = "U represents the unitary group."
SU::usage = "SU represents the algebra of the special unitary group."
SO::usage = "SO represents the algebra of the special orthogonal group."
Sp::usage = "Sp represents the algebra of the symplectic group."

ToExpression[Outer[StringJoin[#1, #2] &, ToString /@ {A, B, C, D, SU, SO, Sp}, ToString /@ Range[32]]]
{E6, E7, E8, G2, F4, U1}

AlgebraClass::usage = 
"AlgebraClass[algebra] yields the series class of an infinite series algebra and the algebra itself for an exceptional algebra."

Irrep::usage = 
"Irrep[algebraClass] is the combined head of an irreducible representation Lie algebra of the class algebraClass."

Weight::usage = 
"Weight[algebraClass] is the combined head of a weight of algebra of the class algebraClass."

RootOmega::usage = 
"RootOmega[algebraClass] is the combined head of a root of algebra of the class algebraClass in the omega basis."

ScalarProduct::usage = "ScalarProduct[irrep1, irrep2] computes the scalar product of irrep1 and irrep2."

OrthogonalBasis::usage = "OrthogonalBasis[weightOrRoot] transforms weightOrRoot into the orthogonal basis."
OmegaBasis::usage      = "OmegaBasis[weightOrRoot] transforms weightOrRoot into the omega basis."
AlphaBasis::usage      = "AlphaBasis[weightOrRoot] transforms weightOrRoot into the alpha basis.  "

OrthogonalSimpleRoots::usage   = "OrthogonalSimpleRoots[algebra] gives the simple roots of algebra in the orthogonal basis."
OrthogonalSimpleCoRoots::usage = "OrthogonalSimpleCoRoots gives the simple coroots of algebra in the orthogonal basis."

OmegaSimpleRoots::usage   = "OmegaSimpleRoots[algebra] gives the simple roots of algebra in the omega or Dynkin basis."

RootOrthogonal::usage = "RootOrthogonal[algebraClass] is the combined head of a root of algebra of the class algebraClass."

WeightOrthogonal::usage = "WeightOrthogonal[algebraClass] is the combined head of a weight of algebra of the class algebraClass in the orthogonal basis."

WeightAlpha::usage = "WeightAlpha[algebraClass] is the combined head of a weight of algebra of the class algebraClass in the alpha basis."

RootAlpha::usage = "RootAlpha[algebraClass] is the combined head of a root of algebra of the class algebraClass in the alpha basis."

Rank::usage = "Rank[expr] gives the rank of the algebra of expr, which can be an algebra, irrep, weight or root."

Bar::usage = "Bar[irrep] give the conjugated irrep."

IrrepPrime::usage = "IrrepPrime[dim] "

ProductIrrep::usage = "ProductIrrep[irreps] gathers irreps of simple Lie algebras to a representation of a product algebra."
ProductAlgebra::usage = "ProductAlgebra  "

Algebra::usage = "Algebra[algebraClass][n] represents a classical algebra. Algebra[expr] gives the algebra of expr."

OrthogonalFundamentalWeights::usage = "OrthogonalFundamentalWeights[algebra] gives the fundamental weights in the orthogonal basis. "

(* Roots *)
RootSystem::usage = "RootSystem[algebra] generates the complete root system of algebra."
PositiveRoots::usage = "PositiveRoots[algebra] gives only the positive roots of the root system of algebra."
NumberOfRoots::usage = "NumberOfRoots[algebra] gives the number of roots in the root system of algebra."
PositiveRootQ::usage = "PositiveRootQ[root] gives True if root is a positive root and False otherwise."
RootLevel::usage = "RootLevel[root] gives the level of the root in its roots system."
HighestRoot::usage = "HighestRoot[algebra] gives the highest root of the roots system of algebra"
NumberOfPositiveRoots::usage = "NumberOfPositiveRoots[algebra] gives the number of positive roots of algebra."

(* Orbits *)
Orbit::usage = "Orbit  "
DimOrbit::usage = "DimOrbit  "
LowestOrbit::usage = "LowestOrbit  "


(* Irreps *)
WeightSystem::usage = "WeightSystem  "
WeightLevel::usage = "WeightLevel  "

Dim::usage = 
"Dim[irrep] computes the dimension of irrep."

DimName::usage = "DimName  "

Index::usage = 
"Index[irrep] computes the index of irrep."

CasimirInvariant::usage = 
"CasimirInvariant[irrep] computes the Casimir invariant of irrep."

Height::usage = "Height  "
WeightMultiplicity::usage = "WeightMultiplicity  "

YoungTableau::usage = 
"YoungTableau[irrep] displays the Young tableau corresponding to irrep of SU(N)"

(* Decompositions *)
DecomposeIrrep::usage = "DecomposeIrrep  "
DecomposeProduct::usage = "DecomposeProduct  "
ProjectionMatrix::usage = "ProjectionMatrix  "

IrrepPlus::usage = "IrrepPlus  "
IrrepTimes::usage = "IrrepTimes  "
IrrepMultiplicity::usage = "IrrepMultiplicity  "
$MaxDynkinDigit::usage = "$MaxDynkinDigit  "
SpindleShape::usage = "SpindleShape  "
SortIrreps::usage = "SortIrreps  "
LaTeXForm::usage = "LaTeXForm  "
IrrepName::usage = "IrrepName  "
IrrepProduct::usage = "IrrepProduct  "
IrrepRule::usage = "IrrepRule  "
ContainsSingletQ::usage = "ContainsSingletQ  "
IrrepQ::usage = "IrrepQ  "
OuterAutomorphisms::usage = "OuterAutomorphisms  "
ConjugateIrrep::usage = "ConjugateIrrep  "
CongruencyClass::usage = "CongruencyClass  "
IrrepOrderedQ::usage = "IrrepOrdering  "
CongruencyVector::usage = "CongruencyVector  "
SingletInDecompositionQ::usage = "SingletInDecompositionQ  "
DimLabel::usage = "DimLabel  "
DynkinLabel::usage = "DynkinLabel  "
ToLaTeX::usage = "ToLaTeX  "
WeylDimensionFormula::usage = "WeylDimensionFormula  "
Delta::usage = "Delta  "
DMatrix::usage = "DMatrix  "
ReflectionMatrices::usage = "ReflectionMatrices  "
Reflect::usage = "Reflect  "
DominantQ::usage = "DominantQ  "
ZeroRoots::usage = "ZeroRoots  "
OmegaMatrix::usage = "OmegaMatrix  "
SingleDominantWeightSystem::usage = "SingleDominantWeightSystem  "
DominantWeightSystem::usage = "DominantWeightSystem  "
XisAndMul::usage = "XisAndMul  "
T::usage = "T  "
Xis::usage = "Xis  "
Alphas::usage = "Alphas  "
WeightDiagram::usage = "WeightDiagram  "
DynkinDiagram::usage = "DynkinDiagram  "
ExtendedDynkinDiagram::usage = "ExtendedDynkinDiagram  "
UnequalPartition::usage = "UnequalPartition  "
ProductWeight::usage = "ProductWeight  "
BasicIrreps::usage = "BasicIrreps  "
GeneratingIrrep::usage = "SimpleIrrep  "
AdjointIrrep::usage = "AdjointIrrep  "
ConjugateIrrepQ::usage = "ConjugateIrrepQ  "
NonConjugateIrrepQ::usage = "NonConjugateIrrepQ  "
RealIrrepQ::usage = "RealIrrepQ  "
ComplexIrrepQ::usage = "ComplexIrrepQ  "
U1Charge::usage = "U1Charge  "
SimpleLieAlgebraQ::usage = "LieAlgebraQ  "
IrrepList::usage = "IrrepList  "
DominantWeightsAndMul::usage = "DominantWeightsAndMul  "
SortOutIrrep::usage = "SortOutIrrep  "
SemiSimpleSubalgebra::usage = "SemiSimpleSubalgebra  "
Project::usage = "Project  "
GroupProjectedWeights::usage = "GroupProjectedWeights  "
SpinorIrrep::usage = "SpinorIrrep  "
HighestWeight::usage = "HighestWeight  "
WeightSystemWithMul::usage = "WeightSystemWithMul  "
GetIrrepByDim::usage = "GetIrrepByDim  "
YoungTableaux::usage = "YoungTableaux  "
TrivialStabilizerWeights::usage = "TrivialStabilizerWeights  "
ReflectToDominantWeightWithMul::usage = "ReflectToDominantWeightWithMul  "
Add::usage = "Add  "
ToIrrep::usage = "ToIrrep  "
DominantWeights::usage = "DominantWeights  "

MaxDynkinDigit::usage = "MaxDynkinDigit  "

ClassicalAlgebraTypes::usage = "ClassicalAlgebraTypes  "

Begin["`Private`"]

(* ::Section:: *)
(* Formatting *)

$BoxForms = {StandardForm, TraditionalForm, LaTeXForm}
ParentForm[LaTeXForm] ^= StandardForm

SetOptions[EvaluationNotebook[], CommonDefaultFormatTypes -> {"Output" -> TraditionalForm}]

ToLaTeX[expr_] := ToString[expr, LaTeXForm]

DisplayLabelCommaSeparated[label__] := StringJoin["(", ToString[Row[ToString[#,InputForm]&/@{label},", "]], ")"]
DisplayLabel[label__] := ToString[Row[{"(", label, ")"}]]

Format[RootOrthogonal[_][label__]]                                  := DisplayLabelCommaSeparated[label]
Format[WeightOrthogonal[_][label__]]                                := DisplayLabelCommaSeparated[label]
Format[RootAlpha[_][label__]]                                       := DisplayLabelCommaSeparated[label]
Format[WeightAlpha[_][label__]]                                     := DisplayLabelCommaSeparated[label]
Format[RootOmega[_][label__]      , StandardForm]                   := DisplayLabelCommaSeparated[label]
Format[Weight[_][label__]?WeightQ , StandardForm]                   := DisplayLabelCommaSeparated[label]
Format[RootOmega[_][label__]      , TraditionalForm]                := Grid[{{label}}, Frame -> True]
Format[RootOmega[_][label__]      , LaTeXForm]                      := ToString["\\rootomega"<>ToString[{label}]]
Format[RootOrthogonal[_][label__] , LaTeXForm]                      := ToString["\\rootorthogonal"<>ToString[{label}]]
Format[Weight[_][label__]?WeightQ , TraditionalForm]                := Grid[{{label}}, Frame -> True]
Format[Weight[_][label__]?WeightQ , LaTeXForm]                      := ToString["\\weight"<>ToString[{label}]]
Format[DynkinLabel[Irrep[_][label__?(0<=#<10&)]?IrrepQ]]            := DisplayLabel[label]
Format[DynkinLabel[Irrep[_][label__]?IrrepQ]]                       := DisplayLabelCommaSeparated[label]
Format[DynkinLabel[Irrep[_][label__?(0<=#<10&)]?IrrepQ], LaTeXForm] := ToString["\\dynkin"<>ToString[{label}]]
Format[DynkinLabel[Irrep[_][label__]?IrrepQ], LaTeXForm]            := ToString["\\dynkincomma"<>ToString[{label}]]

(* Irrep *)
Format[Irrep[U][charge_]]                                             := ToString["("<>ToString[charge,InputForm]<>")"]
Format[Irrep[algebraClass_][label__?(0<=#<10&)]?IrrepQ, StandardForm] := DisplayLabel[label]               
Format[irrep:(Irrep[algebraClass_][label__])?IrrepQ   , StandardForm] := DisplayLabelCommaSeparated[label] 
Format[irrep:(Irrep[_][__])?IrrepQ] 								  := DimName[irrep]

(*CongruencyVector*)
Format[CongruencyVector[label__]] := DisplayLabel[label]

(*ProductIrrep*)
ProductIrrep /: MakeBoxes[ProductIrrep[irreps__?IrrepQ,u1Charge:Irrep[U][_]...], TraditionalForm] :=  
RowBox[{"(", Sequence @@ Riffle[MakeBoxes[#,TraditionalForm] & /@ {irreps}, ","], Sequence @@ Riffle[{")", Sequence @@ (MakeBoxes[#, TraditionalForm]&/@{u1Charge})},"\[InvisibleSpace]"]}]
Format[ProductIrrep[irreps__?IrrepQ,u1Charge:Irrep[U][_]...], StandardForm] :=  
DisplayForm@RowBox[{"(", Sequence @@ Riffle[MakeBoxes[#,StandardForm] & /@ {irreps}, ","], ")",Sequence @@ (MakeBoxes[#, StandardForm]&/@{u1Charge})}]
Format[ProductIrrep[irreps__?IrrepQ,u1charge:Irrep[U][_]...], LaTeXForm]    := ToString[StringJoin["(",ToString[Row[LaTeXForm/@{irreps}, ","]],")",ToString[Row[LaTeXForm/@{u1charge}]]]]

(*IrrepPlus*)
IrrepPlus /: MakeBoxes[IrrepPlus[expr___],TraditionalForm] := RowBox[Riffle[MakeBoxes[#,TraditionalForm]&/@{expr},"+"]]
Format[IrrepPlus[expr___],StandardForm] := DisplayForm@RowBox[Riffle[MakeBoxes[#,StandardForm]&/@{expr},"+"]]

Format[IrrepPlus[expr___], LaTeXForm] := ToString["$"<>ToString[Row[LaTeXForm/@{expr},"+"]]<>"$"]

(*IrrepProduct*)
IrrepProduct /: MakeBoxes[IrrepProduct[irreps__?IrrepQ],TraditionalForm] := RowBox@Riffle[MakeBoxes[#,TraditionalForm]&/@{irreps},"\[CircleTimes]"]
Format[IrrepProduct[irreps__?IrrepQ],StandardForm] := DisplayForm@RowBox@Riffle[MakeBoxes[#,StandardForm]&/@{irreps},"\[CircleTimes]"]
Format[IrrepProduct[irreps__?IrrepQ],LaTeXForm] := ToString["$"<>ToString[Row[LaTeXForm/@{irreps},"\\times"]]<>"$"]
Format[IrrepRule[expr__],LaTeXForm]             := ToString[Row[ToLaTeX/@{expr},"\\rightarrow"]]
IrrepRule /: MakeBoxes[IrrepRule[expr__],TraditionalForm] := RowBox@Riffle[MakeBoxes[#,TraditionalForm]&/@{expr},"\[RightArrow]"]
Format[IrrepRule[expr__],StandardForm]           := DisplayForm@RowBox@Riffle[MakeBoxes[#,StandardForm]&/@{expr},"\[RightArrow]"]

(*IrrepTimes*)
IrrepTimes /: MakeBoxes[IrrepTimes[factor_,irrep_?IrrepQ],TraditionalForm] := RowBox@{MakeBoxes[factor,TraditionalForm],"(",MakeBoxes[irrep,TraditionalForm],")"}
Format[IrrepTimes[factor_,irrep_?IrrepQ],StandardForm]    := DisplayForm@RowBox@{factor,irrep}
Format[IrrepTimes[factor_,irrep_?IrrepQ],LaTeXForm]       := ToString@StringJoin[ToString[factor],"(",ToLaTeX[irrep],")"]

(*IrrepTimes*)
Format[IrrepTimes[1,irrep_?IrrepQ],LaTeXForm]                  := ToString[ToLaTeX[irrep]]
IrrepTimes /: MakeBoxes[IrrepTimes[factor_,irrep_ProductIrrep],TraditionalForm] := RowBox@{MakeBoxes[factor,TraditionalForm],MakeBoxes[irrep,TraditionalForm]}
Format[IrrepTimes[factor_,irrep_ProductIrrep],StandardForm]    := DisplayForm@RowBox@{factor,Format[irrep,StandardForm]}
Format[IrrepTimes[factor_,irrep_ProductIrrep],LaTeXForm]       := ToString[StringJoin[ToString[factor],ToLaTeX[irrep]]]

(*Algebra*)
Format[Algebra[algebraClass_][n_], StandardForm] := Subscript[algebraClass,n]
Format[Algebra[U][1] , TraditionalForm]          := "U(1)"
Format[Algebra[A][n_], TraditionalForm]          := ToString["SU("<>ToString[n+1]<>")"]
Format[Algebra[B][n_], TraditionalForm]          := ToString["SO("<>ToString[2n+1]<>")"]
Format[Algebra[C][n_], TraditionalForm]          := ToString["Sp("<>ToString[2n]<>")"]
Format[Algebra[D][n_], TraditionalForm]          := ToString["SO("<>ToString[2n]<>")"]
Format[Algebra[U][1] , LaTeXForm] := "U(1)"
Format[Algebra[A][n_], LaTeXForm] := ToString["SU("<>ToString[n+1]<>")"]
Format[Algebra[B][n_], LaTeXForm] := ToString["SO("<>ToString[2n+1]<>")"]
Format[Algebra[C][n_], LaTeXForm] := ToString["Sp("<>ToString[2n]<>")"]
Format[Algebra[D][n_], LaTeXForm] := ToString["SO("<>ToString[2n]<>")"]
Format[E6] := Subscript["E",6]
Format[E7] := Subscript["E",7]
Format[E8] := Subscript["E",8]
Format[F4] := Subscript["F",4]
Format[G2] := Subscript["G",2]
Format[E6, LaTeXForm] := "\\E6"
Format[E7, LaTeXForm] := "\\E7"
Format[E8, LaTeXForm] := "\\E8"
Format[F4, LaTeXForm] := "\\F4"
Format[G2, LaTeXForm] := "\\G2"

(*ProductAlgebra*)
Format[ProductAlgebra[algebra_]] := algebra
ProductAlgebra /: MakeBoxes[ProductAlgebra[algebras__],TraditionalForm] := RowBox@Riffle[MakeBoxes[#,TraditionalForm]&/@{algebras},"\[Times]"]
Format[ProductAlgebra[algebras__],StandardForm] := DisplayForm@RowBox@Riffle[MakeBoxes[#,StandardForm]&/@{algebras},"\[Times]"]
Format[ProductAlgebra[algebras__], LaTeXForm]  := ToString[Row[ToLaTeX/@{algebras},"${\\times}$"]]

(*IrrepName*)
DimLabel[dim_, 0, ""]         := dim
DimLabel[dim_, 0, sub_]       := SubscriptBox[dim, sub]
DimLabel[dim_, primes_, ""]   := SuperscriptBox[dim, StringJoin @@ ConstantArray["\[Prime]", primes]]
DimLabel[dim_, primes_, sub_] := SubsuperscriptBox[dim, sub, StringJoin @@ ConstantArray["\[Prime]", primes]]

Format[irrep : IrrepName[algebra_][dim_, conjugated_: False, primes_: 0, sub_: ""], LaTeXForm] := 
    StringJoin[
        "\\irrep",
        If[conjugated, "bar", ""], 
        If[sub =!= "", "sub", ""],
        If[primes > 0, "["<>ToString[primes]<>"]",""],
        "{" <> ToString[dim] <> "}",
        If[sub =!= "", "{" <> ToString[sub] <> "}", ""]
    ]    

IrrepName /: MakeBoxes[irrep : IrrepName[algebra_][dim_, conjugated_: False, primes_: 0, sub_: ""],TraditionalForm] := 
DimLabel[If[conjugated, StyleBox[OverscriptBox[ToString[dim],"_"],Bold], StyleBox[ToString[dim],Bold]], primes, sub]


(* ::Section:: *)
(* Algebras *)


$MaxDynkinDigit = 3

Outer[(If[SameQ[Head[algebra=Evaluate[ToExpression[StringJoin[ToString[#1], ToString[#2]]]]],Symbol], Evaluate[algebra] = Algebra[#1][#2]]) &, {A, B, C, D, SU, SO}, Range[32]]
Outer[(If[SameQ[Head[algebra=Evaluate[ToExpression[StringJoin[ToString[#1], ToString[#2]]]]],Symbol], Evaluate[algebra] = Algebra[#1][#2]]) &, {Sp}, Range[2, 32, 2]]

NaturalNumberQ[expr_] := IntegerQ[expr] && Positive[expr]
OddNaturalNumberQ[expr_] := OddQ[expr] && NaturalNumberQ[expr]
EvenNaturalNumberQ[expr_] := EvenQ[expr] && NaturalNumberQ[expr]

ClassicalAlgebraTypes = (A|B|C|D)
ExceptionalAlgebras = (E6|E7|E8|F4|G2)
LieAlgebraTypesQ =  ClassicalAlgebraTypes | ExceptionalAlgebras

InfiniteSeriesAlgebras = Algebra[ClassicalAlgebraTypes][_?NaturalNumberQ]

SimpleLieAlgebraQ[expr_] := MatchQ[expr, InfiniteSeriesAlgebras | ExceptionalAlgebras | Algebra[U][1]]
ProductLieAlgebraQ[ProductAlgebra[algebras__?SimpleLieAlgebraQ]] := True
LieAlgebraQ[_?SimpleLieAlgebraQ|_?ProductLieAlgebraQ] := True

DynkinLabelQ[expr__] := And @@ (And[IntegerQ[#], NonNegative[#]] & /@ {expr})

LabelQ[expr__] := And @@ (IntegerQ /@ {expr})

DimNameQ[expr_] := IntegerQ[expr] || Head[expr]===Bar || Head[expr]===IrrepPrime  

Attributes[Rank] = {Listable}
Rank[(WeightOrthogonal|RootOrthogonal)[A][label__]] := Length[{label}] - 1
Rank[(WeightOrthogonal|RootOrthogonal)[B|C|D][label__]] := Length[{label}]
Rank[(Irrep|Weight|WeightAlpha|RootOmega|RootAlpha)[A|B|C|D][label__]] := Length[{label}]
Rank[_[algebraClass:ExceptionalAlgebras][__]] := Rank[algebraClass]
Rank[Algebra[algebraClass_][n_]] := n
Rank[E6] = 6
Rank[E7] = 7
Rank[E8] = 8
Rank[F4] = 4
Rank[G2] = 2
Rank[productAlgebra_ProductAlgebra] := Total[Rank/@productAlgebra]

ExceptionalAlgebraPattern[head_, algebra:ExceptionalAlgebras] := With[{head=head, n=Rank[algebra]},HoldPattern[head[algebra][Repeated[_, {n}]?LabelQ]]]
ExceptionalAlgebraPattern[algebra:ExceptionalAlgebras] := With[{n=Rank[algebra]},HoldPattern[Irrep[algebra][Repeated[_, {n}]?DynkinLabelQ]]]
NonNumericExceptionalAlgebraPattern[algebra:ExceptionalAlgebras] := With[{n=Rank[algebra]},HoldPattern[Irrep[algebra][Repeated[_, {n}]]]]

U1ChargePattern = Irrep[U][_]
IrrepPattern = With[{algebraClasses=ClassicalAlgebraTypes}, HoldPattern[Irrep[U][_?NumberQ] | Irrep[algebraClasses][__?DynkinLabelQ]]] | (Sequence @@ ExceptionalAlgebraPattern[#]& /@ ExceptionalAlgebras)
NonNumericIrrepPattern= With[{algebraClasses=ClassicalAlgebraTypes}, HoldPattern[U1ChargePattern | Irrep[algebraClasses][__]]] | (Sequence @@ NonNumericExceptionalAlgebraPattern[#]& /@ ExceptionalAlgebras)

HeadAlgebraPattern[head_] := HeadAlgebraPattern[head] = With[{head=head, algebraClasses=ClassicalAlgebraTypes},HoldPattern[head[U][_?NumberQ] | head[algebraClasses][__?LabelQ]]] | (Sequence @@ ExceptionalAlgebraPattern[head, #]& /@ ExceptionalAlgebras)

IrrepQ[irrep_]  := MatchQ[irrep, IrrepPattern]
ProductIrrepPattern = ProductIrrep[__?IrrepQ]

ProductIrrepQ[expr_] := MatchQ[expr,ProductIrrepPattern]
SingleOrProductIrrepQ[expr_] := MatchQ[expr,IrrepPattern|ProductIrrepPattern]
U1ChargeQ[expr_] := MatchQ[expr,U1ChargePattern]

NonNumericIrrepQ[expr_] := MatchQ[expr, NonNumericIrrepPattern]
WeightQ[expr_] := MatchQ[expr, HeadAlgebraPattern[Weight]]
RootOmegaQ[expr_] := MatchQ[expr, HeadAlgebraPattern[RootOmega]]

U1 = Algebra[U][1]
SU[n_?NaturalNumberQ]     := Algebra[A][n-1]
SO[n_?OddNaturalNumberQ]  := Algebra[B][(n-1)/2]
SO[n_?EvenNaturalNumberQ] := Algebra[D][n/2]
Sp[n_?EvenNaturalNumberQ] := Algebra[C][n/2]

Algebra[SU][n_?NaturalNumberQ]     := Algebra[A][n-1]
Algebra[SO][n_?OddNaturalNumberQ]  := Algebra[B][(n-1)/2]
Algebra[SO][n_?EvenNaturalNumberQ] := Algebra[D][n/2]
Algebra[Sp][n_?EvenNaturalNumberQ] := Algebra[C][n/2]

IR = Irrep

EmptyQ[list_] := MatchQ[list, {{} ...}]
NonEmptyQ[list_] := !MatchQ[list, {{} ...}]

RootOrthogonal   /: Dot[RootOrthogonal[_][a__], RootOrthogonal[_][b__]] := Dot[{a}, {b}]
RootOrthogonal   /: Dot[RootOrthogonal[_][a__], b_List] := Dot[{a}, b]
RootOrthogonal   /: RootOrthogonal[algebraClass_][label1__] + RootOrthogonal[algebraClass_][label2__] := RootOrthogonal[algebraClass] @@ ({label1} + {label2})
RootOrthogonal   /: f_ * RootOrthogonal[algebraClass_][label__] := RootOrthogonal[algebraClass] @@ (f * {label})
RootOmega        /: f_ * RootOmega[algebraClass_][label__] := RootOmega[algebraClass] @@ (f * {label})
RootOmega        /: RootOmega[algebraClass_][label1__] + RootOmega[algebraClass_][label2__] := RootOmega[algebraClass] @@ ({label1} + {label2})
WeightAlpha      /: f_ * WeightAlpha[algebraClass_][label__] := WeightAlpha[algebraClass] @@ (f * {label})
WeightAlpha      /: WeightAlpha[algebraClass_][label1__] + WeightAlpha[algebraClass_][label2__] := WeightAlpha[algebraClass] @@ ({label1} + {label2})
Weight           /: f_ * Weight[algebraClass_][label__] := Weight[algebraClass] @@ (f * {label})
Weight           /: Weight[algebraClass_][label1__] + Weight[algebraClass_][label2__] := Weight[algebraClass] @@ ({label1} + {label2})
Weight           /: Weight[algebraClass_][label1__] + Irrep[algebraClass_][label2__] := Weight[algebraClass] @@ ({label1} + {label2})
Weight           /: Weight[algebraClass_][label1__] + RootOmega[algebraClass_][label2__] := Weight[algebraClass] @@ ({label1} + {label2})
WeightOrthogonal /: WeightOrthogonal[algebraClass_][label1__] + RootOrthogonal[algebraClass_][label2__] := WeightOrthogonal[algebraClass] @@ ({label1} + {label2})

Seek[expr_List, condition_] := Catch[If[condition@#, Throw[#]] & /@ expr; Null]

GeneratingIrrep[Algebra[algebraClass:A|C][n_?NaturalNumberQ]] := Irrep[algebraClass]@@PadRight[{1},n]
GeneratingIrrep[Algebra[algebraClass:B|D][n_?NaturalNumberQ]] := Irrep[algebraClass]@@PadLeft[{1},n]
GeneratingIrrep[algebra:E6|G2] := Irrep[algebra]@@PadRight[{1} ,Rank[algebra]]
GeneratingIrrep[algebra:F4]    := Irrep[algebra]@@PadLeft[{1}  ,Rank[algebra]]
GeneratingIrrep[algebra:E7|E8] := Irrep[algebra]@@PadLeft[{1,0},Rank[algebra]]

SpinorIrrep[Algebra[algebraClass:B|D][n_?NaturalNumberQ]] := Irrep[algebraClass]@@PadLeft[{1},n]

AdjointIrrep[algebra_?SimpleLieAlgebraQ] := Irrep[AlgebraClass[algebra]]@@HighestRoot[algebra]

BasicIrreps[algebra_?SimpleLieAlgebraQ] := Irrep[AlgebraClass[algebra]]@@@IdentityMatrix[Rank[algebra]]

(* distinct irreps with the same dim & Dynkin index*)
Attributes[OuterAutomorphisms] = {Listable}
OuterAutomorphisms[irrep : Irrep[U][u1charge_]] :=
    If[u1charge==0,{},{Irrep[U][-u1charge]}]
OuterAutomorphisms[irrep_?IrrepQ] :=
    Select[
        Irrep[AlgebraClass[irrep]] @@@ Tuples[Range[0, Max[List @@ irrep]], Rank[irrep]], 
        # =!= irrep && Dim[#] == Dim[irrep] && Index[#] == Index[irrep] &
    ]

(* real OR QUATERNIONIC irrep (should this phrasing be fixed?) *)
RealIrrepQ[irrep : Irrep[U][u1charge_]] := u1charge==0
RealIrrepQ[irrep_?IrrepQ] :=
    IrrepMultiplicity[DecomposeProduct[irrep,irrep], Irrep[Algebra[irrep]][1]] > 0
ComplexIrrepQ[irrep_?IrrepQ] := Not[RealIrrepQ[irrep]]

Attributes[ConjugateIrrep] = {Listable}
ConjugateIrrep[irrep : Irrep[U][u1charge_]] := Irrep[U][-u1charge]
ConjugateIrrep[irrep_?IrrepQ] := If[RealIrrepQ[irrep], irrep, First @ OuterAutomorphisms[irrep]]

ConjugateIrrep[productIrreps_?ProductIrrepQ] := ConjugateIrrep /@ productIrreps
Attributes[Bar] = {Listable}
Bar[productIrreps_?ProductIrrepQ] := Bar /@ productIrreps 
Bar[irrep_?IrrepQ] := ConjugateIrrep[irrep]  
Bar[irrep : Irrep[U][u1charge_]] := Irrep[U][-u1charge]

ConjugateIrrepQ[irrep_?IrrepQ] := DimName[irrep][[2]]
NonConjugateIrrepQ[irrep_?IrrepQ] := !DimName[irrep][[2]]

IrrepPrime[dim_] := IrrepPrime[dim, 1]
IrrepPrime[dim_, 0] := dim
Bar[Bar[dim_?NumberQ]] := dim
Bar[IrrepPrime[dim_], 1] := IrrepPrime[Bar[dim]]
Bar[IrrepPrime[dim_, numprimes_]] := IrrepPrime[Bar[dim], numprimes]

Irrep/: Power[irrep:Irrep[_][__]?IrrepQ,k_?NaturalNumberQ]    := DecomposeProduct@@ConstantArray[irrep,k]

Irrep/: Times[irrep:Irrep[_][__]?IrrepQ,irreps:Irrep[_][__]?IrrepQ..] := DecomposeProduct[irrep, irreps]
Irrep/: Times[factor_?NaturalNumberQ,irreps:Irrep[_][__]?IrrepQ..]    := IrrepTimes[factor, irreps]
Irrep/: Dot[irrep:Irrep[_][__]?IrrepQ,irreps:Irrep[_][__]?IrrepQ..]   := DecomposeProduct[irrep, irreps]
Irrep/: Dot[factor_?NaturalNumberQ,irreps:Irrep[_][__]?IrrepQ..]      := IrrepTimes[factor, irreps]
Irrep/: Plus[irrep:Irrep[_][__]?IrrepQ,irreps:Irrep[_][__]?IrrepQ..]  := IrrepPlus[irrep, irreps]

ProductIrrep/: Times[irrep:ProductIrrep[__]?ProductIrrepQ, irreps:ProductIrrep[__]?ProductIrrepQ..] := DecomposeProduct[irrep, irreps]
ProductIrrep/: Times[factor_?NaturalNumberQ,irreps:ProductIrrep[__]?ProductIrrepQ..]                := IrrepTimes[factor, irreps]
ProductIrrep/: Dot[irrep:ProductIrrep[__]?ProductIrrepQ, irreps:ProductIrrep[__]?ProductIrrepQ..]   := DecomposeProduct[irrep, irreps]
ProductIrrep/: Dot[factor_?NaturalNumberQ,irreps:ProductIrrep[__]?ProductIrrepQ..]                  := IrrepTimes[factor, irreps]
ProductIrrep/: Plus[irrep:Irrep[_][__]?IrrepQ,irreps:ProductIrrep[__]?ProductIrrepQ..]              := IrrepPlus[irrep, irreps]

U1Charge[ProductIrrep[___,Irrep[U][u1charge_]]?ProductIrrepQ]:= u1charge

IrrepOrderedQ[irreps_ProductIrrep ,irreps_ProductIrrep]  := True
IrrepOrderedQ[irreps1_ProductIrrep,irreps2_ProductIrrep] := Total[Dim[irreps1]]<=Total[Dim[irreps2]]/;Total[Dim[irreps1]]!=Total[Dim[irreps2]]
IrrepOrderedQ[irreps1_ProductIrrep,irreps2_ProductIrrep] := Total[Index[irreps1]]<=Total[Index[irreps2]]/;Total[Dim[irreps1]]==Total[Dim[irreps2]]&&Total[Index[irreps1]]!=Total[Index[irreps2]]
IrrepOrderedQ[irreps1_ProductIrrep,irreps2_ProductIrrep] := Total[CongruencyClass[irreps1]]<=Total[CongruencyClass[irreps2]]/;(Total[Dim[irreps1]]==Total[Dim[irreps2]]&&Total[Index[irreps1]]==Total[Index[irreps2]]&&Total[CongruencyClass[irreps1]]!=Total[CongruencyClass[irreps2]])
IrrepOrderedQ[irreps1_ProductIrrep,irreps2_ProductIrrep] := OrderedQ[{irreps2,irreps1}]/;(Total[Dim[irreps1]]==Total[Dim[irreps2]]&&Total[Index[irreps1]]==Total[Index[irreps2]]&&Total[CongruencyClass[irreps1]]==Total[CongruencyClass[irreps2]])
 
IrrepOrderedQ[irrep_,irrep_]   := True
IrrepOrderedQ[irrep1_,irrep2_] := Dim[irrep1]<=Dim[irrep2]/;Dim[irrep1]!=Dim[irrep2]
IrrepOrderedQ[irrep1_,irrep2_] := Index[irrep1]<=Index[irrep2]/;Dim[irrep1]==Dim[irrep2]&&Index[irrep1]!=Index[irrep2]
IrrepOrderedQ[irrep1_,irrep2_] := Norm[CongruencyClass[irrep1]]<=Norm[CongruencyClass[irrep2]]/;(Dim[irrep1]==Dim[irrep2]&&Index[irrep1]==Index[irrep2]&&Norm[CongruencyClass[irrep1]]!=Norm[CongruencyClass[irrep2]])
IrrepOrderedQ[irrep1_,irrep2_] := OrderedQ[{irrep2,irrep1}]/;(Dim[irrep1]==Dim[irrep2]&&Index[irrep1]==Index[irrep2]&&Norm[CongruencyClass[irrep1]]==Norm[CongruencyClass[irrep2]])

SortIrreps[irrepList_List]:= Sort[irrepList, IrrepOrderedQ]

IrrepList[IrrepPlus[irrepterms__]]:= Flatten[IrrepList/@{irrepterms}]
IrrepList[irrep_?IrrepQ|irrep_?ProductIrrepQ]:= irrep
IrrepList[IrrepTimes[factor_?NaturalNumberQ, irrep_?SingleOrProductIrrepQ]]:= ConstantArray[irrep,factor]

IrrepPlus[{irreps__?SingleOrProductIrrepQ}] := IrrepPlus@@(IrrepTimes[#2, #1]& @@@ Sort[Tally[{irreps}], IrrepOrderedQ[#1[[1]], #2[[1]]] &])

IrrepPlus[irrepSums__IrrepPlus]:= 
    Module[{},
        IrrepPlus@@(IrrepTimes[#2, #1]& @@@ Sort[
            {#[[1, 1]], Total[#[[All, 2]]]} & /@
                GatherBy[Replace[Flatten[List@@@{irrepSums}], (irrep_?IrrepQ|irrep_?ProductIrrepQ) :> {irrep, 1}, 1] /.IrrepTimes[factor_?IntegerQ, irrep_?IrrepQ|irrep_?ProductIrrepQ] :> {irrep, factor}, First]
        , IrrepOrderedQ[#1[[1]], #2[[1]]] &])
    ]
IrrepPlus/: Times[factor_?NaturalNumberQ,IrrepPlus[terms__]] := IrrepPlus@@IrrepTimes[factor,{terms}]

IrrepMultiplicity[decomposition_IrrepPlus, irrep_?SingleOrProductIrrepQ] :=  decomposition /. IrrepPlus[___, IrrepTimes[multiplicity_, irrep], ___] :> multiplicity/. IrrepPlus[___, irrep, ___] :> 1 /. IrrepPlus[___] :> 0

Attributes[IrrepTimes]:={Listable}
IrrepTimes[1,irrep_?SingleOrProductIrrepQ] := irrep
IrrepTimes[factor_?NaturalNumberQ,IrrepPlus[terms__]]:=IrrepPlus@@IrrepTimes[factor,{terms}]
IrrepTimes[factor1_?NaturalNumberQ,IrrepTimes[factor2_?NaturalNumberQ,irrep_?SingleOrProductIrrepQ]]:=IrrepTimes[factor1*factor2,irrep]

ToMatrix[expr_] := List @@@ expr

Attributes[Algebra] = {Listable}
Algebra[irrep:(Weight|WeightAlpha|Irrep|RootOmega|RootAlpha|RootOrthogonal|WeightOrthogonal)[algebra:ExceptionalAlgebras][__]] := algebra
Algebra[irrep:(Weight|WeightAlpha|Irrep|RootOmega|RootAlpha|RootOrthogonal|WeightOrthogonal)[algebraClass:(A|B|C|D)][label__]] := Algebra[algebraClass][Rank[irrep]]

Attributes[AlgebraClass] = {Listable}
AlgebraClass[Algebra[algebraClass_][n_]] := algebraClass
AlgebraClass[algebra_Symbol] := algebra 
AlgebraClass[irrep:(Weight|WeightAlpha|Irrep|RootOmega|RootAlpha|RootOrthogonal|WeightOrthogonal)[algebra:ExceptionalAlgebras][__]] := algebra
AlgebraClass[irrep:(Weight|WeightAlpha|Irrep|RootOmega|RootAlpha|RootOrthogonal|WeightOrthogonal)[algebraClass:(A|B|C|D)][label__]] := algebraClass
AlgebraClass[ProductIrrep[irreps__?IrrepQ]] := AlgebraClass[{irreps}]
AlgebraClass[ProductAlgebra[algebras__?LieAlgebraQ]] := AlgebraClass[{algebras}]

OrthogonalSimpleRoots[Algebra[A][n_?NaturalNumberQ]] := OrthogonalSimpleRoots[Algebra[A][n]] = RootOrthogonal[A] @@@ Normal[SparseArray[{Band[{1, 1}] -> 1, Band[{1, 2}] -> -1}, {n, n + 1}]]
OrthogonalSimpleRoots[Algebra[B][n_?NaturalNumberQ]] := OrthogonalSimpleRoots[Algebra[B][n]] = RootOrthogonal[B] @@@ Normal[SparseArray[{Band[{1, 1}] -> 1, Band[{1, 2}] -> -1}, {n, n}]]
OrthogonalSimpleRoots[Algebra[C][n_?NaturalNumberQ]] := OrthogonalSimpleRoots[Algebra[C][n]] = RootOrthogonal[C] @@@ Normal[SparseArray[{Band[{n, n}] -> 2, Band[{1, 1}] -> 1, Band[{1, 2}] -> -1}, {n, n}]]
OrthogonalSimpleRoots[Algebra[D][n_?NaturalNumberQ]] := OrthogonalSimpleRoots[Algebra[D][n]] = RootOrthogonal[D] @@@ Normal[SparseArray[{Band[{n, n - 1}] -> 1, Band[{1, 1}] -> 1, Band[{1, 2}] -> -1}, {n, n}]]
ExceptionalOrthogonalSimpleRoots[n_] := Normal[Prepend[SparseArray[{{n - 1, 1} -> 1, {n - 1, 2} -> 1, Band[{1, 1}, {n - 2, n - 2}] -> -1, Band[{1, 2}, {n - 1, n - 1}] -> 1}, {n - 1, 8}], Flatten[{1/2, ConstantArray[-1/2, 6], 1/2}]]]
OrthogonalSimpleRoots[E6] = RootOrthogonal[E6] @@@ ExceptionalOrthogonalSimpleRoots[6]
OrthogonalSimpleRoots[E7] = RootOrthogonal[E7] @@@ ExceptionalOrthogonalSimpleRoots[7]
OrthogonalSimpleRoots[E8] = RootOrthogonal[E8] @@@ ExceptionalOrthogonalSimpleRoots[8]
OrthogonalSimpleRoots[F4] = RootOrthogonal[F4] @@@ {{1, -1, 0, 0}, {0, 1, -1, 0}, {0, 0, 1, 0}, {-1/2, -1/2, -1/2, -1/2}}
OrthogonalSimpleRoots[G2] = RootOrthogonal[G2] @@@ Reverse[{{-2, 1, 1},{1, -1, 0}}]

OrthogonalSimpleCoRoots[algebra_?SimpleLieAlgebraQ] := OrthogonalSimpleCoRoots[algebra] = 2 #/ScalarProduct[#, #] & /@ OrthogonalSimpleRoots[algebra]

CartanMatrix[algebra_?SimpleLieAlgebraQ] := CartanMatrix[algebra]        = 2 Outer[ScalarProduct[#1,#2]/ScalarProduct[#2,#2] &, #, #, 1] &@ OrthogonalSimpleRoots[algebra]
InverseCartanMatrix[algebra_ ]     := InverseCartanMatrix[algebra] = Inverse[CartanMatrix[algebra]]

OmegaMatrix[algebra_?SimpleLieAlgebraQ]        := OmegaMatrix[algebra]        = Transpose @ PseudoInverse @  ToMatrix @ OrthogonalSimpleCoRoots[algebra]
InverseOmegaMatrix[algebra_?SimpleLieAlgebraQ] := InverseOmegaMatrix[algebra] = Transpose @ ToMatrix @ OrthogonalSimpleCoRoots[algebra]

OrthogonalFundamentalWeights[algebra_?SimpleLieAlgebraQ]        := OrthogonalFundamentalWeights[algebra]        =  WeightOrthogonal[AlgebraClass[algebra]] @@@ OmegaMatrix[algebra]
InverseOrthogonalFundamentalWeights[algebra_] := InverseOrthogonalFundamentalWeights[algebra] =  WeightOrthogonal[AlgebraClass[algebra]] @@@ InverseOmegaMatrix[algebra]

NormalizeSimpleRoots[roots_] := roots Sqrt[2/Max[# . # & /@ roots]]

DMatrix[algebra_?SimpleLieAlgebraQ] := DiagonalMatrix[ScalarProduct[#,#]/2 & /@ NormalizeSimpleRoots[OrthogonalSimpleRoots[algebra]]]
SimpleRootLengthsFactor[algebra_] := DiagonalMatrix[2/ScalarProduct[#,#] & /@ NormalizeSimpleRoots[OrthogonalSimpleRoots[algebra]]]

MetricTensor[algebra_?SimpleLieAlgebraQ] := MetricTensor[algebra] = InverseCartanMatrix[algebra] . DMatrix[algebra]

ScalarProduct[weight1: (Weight|Irrep|RootOmega|RootAlpha)[algebraClass_][label1__], weight2: (Weight|Irrep|RootOmega|RootAlpha)[algebraClass_][label2__]] := 
    Dot[{label1}, MetricTensor[Algebra[weight1]], {label2}]
    
ScalarProduct[weight1: (WeightOrthogonal|RootOrthogonal)[algebraClass_][label1__], weight2: (WeightOrthogonal|RootOrthogonal)[algebraClass_][label2__]] := 
    Dot[{label1}, {label2}]



(* ::Section:: *)
(* Bases *)


StandardBasisN[A, m_] := m - 1
StandardBasisN[B|C|D, m_] := m

HeadOrthogonal[Weight]      = WeightOrthogonal
HeadAlpha[Weight]           = WeightAlpha
HeadOmega[WeightOrthogonal] = Weight
HeadOmega[WeightAlpha]      = Weight
HeadOrthogonal[RootOmega]   = RootOrthogonal
HeadAlpha[RootOmega]        = RootAlpha
HeadOmega[RootOrthogonal]   = RootOmega
HeadOmega[RootAlpha]        = RootOmega

Attributes[OrthogonalBasis] = {Listable}
Attributes[AlphaBasis] = {Listable}
Attributes[OmegaBasis] = {Listable}

OrthogonalBasis[weightOrRoot:(WeightOrthogonal|RootOrthogonal)[_][__]] := weightOrRoot
OmegaBasis[weightOrRoot:(Weight|RootOmega)[_][__]] := weightOrRoot
AlphaBasis[weightOrRoot:(WeightAlpha|RootAlpha)[_][__]] := weightOrRoot

OrthogonalBasis[weightOrRoot: (type:(Weight|RootOmega))[algebraClass_][label__]] := 
    HeadOrthogonal[type][algebraClass] @@ Dot[{label}, OmegaMatrix@Algebra@weightOrRoot]
    
OrthogonalBasis[weightOrRoot: (type:(WeightAlpha|RootAlpha))[algebraClass_][label__]] := 
    OrthogonalBasis@OmegaBasis@weightOrRoot
    
AlphaBasis[weightOrRoot: (type:(Weight|RootOmega))[algebraClass_][label__]] := 
    HeadAlpha[type][algebraClass]      @@ Dot[{label}, InverseCartanMatrix@Algebra@weightOrRoot]

AlphaBasis[weightOrRoot: (type:(WeightOrthogonal|RootOrthogonal))[algebraClass_][m__]] :=
    AlphaBasis@OmegaBasis@weightOrRoot

OmegaBasis[weightOrRoot: (type:(WeightAlpha|RootAlpha))[algebraClass_][label__]] := 
    HeadOmega[type][algebraClass]      @@ Dot[{label}, CartanMatrix@Algebra@weightOrRoot]

OmegaBasis[weightOrRoot: (type:(WeightOrthogonal|RootOrthogonal))[algebraClass_][m__]] := 
    HeadOmega[type][algebraClass]      @@ Dot[{m}, InverseOmegaMatrix[Algebra[algebraClass][StandardBasisN[algebraClass, Length[{m}]]]]]

OmegaBasis[weightOrRoot: (type:(WeightOrthogonal|RootOrthogonal))[algebraClass:ExceptionalAlgebras][m__]] := 
    HeadOmega[type][algebraClass]        @@ Dot[{m}, InverseOmegaMatrix[algebraClass]]

Basis[(Weight|WeightAlpha|Irrep|RootOmega|RootAlpha|RootOrthogonal|WeightOrthogonal)[_][__]] := OrthogonalBasis
Basis[(Weight|Irrep|RootOmega)[_][__]] := OmegaBasis
Basis[(WeightAlpha|RootAlpha)[_][__]] := AlphaBasis



(* ::Section:: *)
(* Roots *)


Rbar[algebra_] := Rbar[algebra] = 2 Total /@ InverseCartanMatrix[algebra]

RootLevel[root:RootOmega[_][label__]] := 1/2 Rbar[Algebra[root]] . {label}

ZeroRoots[algebra_] := RootOmega[AlgebraClass[algebra]] @@@ ConstantArray[0, {Rank[algebra], Rank[algebra]}]

Options[RootSystem] = { SpindleShape -> False }
RootSystem[algebra_?SimpleLieAlgebraQ, opts:OptionsPattern[]] := 
RootSystem[algebra, opts] = 
    If[OptionValue[SpindleShape],
        Column[Row[#, " "] & /@ GatherBy[RootSystem[algebra], -RootLevel[#] &], Alignment->Center]
    ,
        SortBy[#,-RootLevel[#]&]& @ Join[ OmegaBasis /@ (Union @@ Orbit /@ OrthogonalSimpleRoots[algebra]), ZeroRoots[algebra]]
    ]

PositiveQ[list_] := And @@ NonNegative[List@@list]

PositiveRootQ[root:(RootOmega|RootAlpha|RootOrthogonal)[_][__]] := MatchQ[List@@AlphaBasis[root], {_?NonNegative ..., _?Positive .., _?NonNegative ...}]

DominantQ[(Weight|RootOmega)[_][label__]]             := PositiveQ[{label}]
DominantQ[weightOrRoot:(WeightOrthogonal|RootOrthogonal|WeightAlpha|RootAlpha)[_][__]] := DominantQ[OmegaBasis[weightOrRoot]]

HighestRoot[algebra_?SimpleLieAlgebraQ] := First[PositiveRoots[algebra]]

NumberOfPositiveRoots[Algebra[A][n_]]   := NumberOfPositiveRoots[Algebra[A][n]]   = n*(n+1)/2
NumberOfPositiveRoots[Algebra[B|C][n_]] := NumberOfPositiveRoots[Algebra[B|C][n]] = n^2
NumberOfPositiveRoots[Algebra[D][n_]]   := NumberOfPositiveRoots[Algebra[D][n]]   = n*(n-1)
NumberOfPositiveRoots[E6] = 36
NumberOfPositiveRoots[E7] = 63
NumberOfPositiveRoots[E8] = 120
NumberOfPositiveRoots[F4] = 24
NumberOfPositiveRoots[G2] = 6

PositiveRoots[algebra_?SimpleLieAlgebraQ]    := PositiveRoots[algebra] = Take[RootSystem[algebra], NumberOfPositiveRoots[algebra]]

NumberOfRoots[algebra_?SimpleLieAlgebraQ] := Length[RootSystem[algebra]]



(* ::Section:: *)
(* Dynkin Diagrams *)


doubleLine[{p1:{x1_,y1_},p2:{x2_,y2_}},rest___]:=Module[{offset={0,0.1}},{Line[{p1+offset,p2+offset}],Line[{p1-offset,p2-offset}]}]
tripleLine[{p1:{x1_,y1_},p2:{x2_,y2_}},rest___]:=Module[{offset={0,0.1}},{Line[{p1+offset,p2+offset}],Line[{p1,p2}],Line[{p1-offset,p2-offset}]}]

edgeStyle[i_,j_,value_]:=
Module[
    {absValue=Abs[value]},
    If[And[i!=j,absValue>1],
        Rule[UndirectedEdge[i,j],If[absValue==2,doubleLine,tripleLine]],
        Nothing
    ]
]

rootColor[rootLengthFactor_]:=If[rootLengthFactor==1,White,Black]

vertexLabels[algebra_]:={Placed["Name",Below]}
vertexLabels[algebra:E6|E7|E8]:={Placed["Name",Below],Rank[algebra]->Placed["Name",After]}

vertexCoordinates[algebra_]:=Array[{#,0}&,Rank[algebra]]
vertexCoordinates[Algebra[D][rank_]]:=Join[Array[{#,0}&,rank-2],{{rank-1,0.5},{rank-1,-0.5}}]
vertexCoordinates[algebra:E6|E7|E8]:=Module[{rank=Rank[algebra]},Append[Array[{#,0}&,rank-1],{3,1}]]
vertexCoordinates[G2]:={{2,0},{1,0}}

Options[DynkinDiagram]={VertexSize->0.3};
DynkinDiagram[algebra_, opts:OptionsPattern[]]:=
Module[
    {
        vertexStyle=MapIndexed[Rule[First[#2],rootColor[#1]]&,Diagonal[DMatrix[algebra]]],
        edgeShapeFunction=Flatten[MapIndexed[edgeStyle[First[#2],Last[#2],#1]&,CartanMatrix[algebra],{2}]]
    },
    KirchhoffGraph[
        CartanMatrix[algebra],
        VertexStyle->vertexStyle,
        VertexLabels->vertexLabels[algebra],
        VertexCoordinates->vertexCoordinates[algebra],
        VertexSize->0.3,
        EdgeStyle->Black,
        EdgeShapeFunction->edgeShapeFunction,
        DirectedEdges->False,
        FilterRules[{opts,Options[DynkinDiagram]}, Options[KirchhoffGraph]]
    ]
]

horizontalScale[algebra_]:=Rank[algebra]
horizontalScale[E6]:=5.2
horizontalScale[E7]:=6.3
horizontalScale[E8]:=7.4
horizontalScale[Algebra[D][n_]]:=n-1.18
horizontalScale[F4]:=4
horizontalScale[G2]:=1.65

vertexLabelsExtended[algebra_]:={Placed["Name",Below],Rank[algebra]+1->Placed["-\[Gamma]",Below]}
vertexLabelsExtended[Algebra[A][rank_]]:={Placed["Name",Below],rank+1->Placed["-\[Gamma]",Above]}
vertexLabelsExtended[E6]:={Placed["Name",Below],6->Placed["Name",After],7->Placed["-\[Gamma]",After]}
vertexLabelsExtended[E7]:={Placed["Name",Below],7->Placed["Name",After],8->Placed["-\[Gamma]",Below]}
vertexLabelsExtended[E8]:={Placed["Name",Below],8->Placed["Name",After],9->Placed["-\[Gamma]",Below]}

vertexCoordinatesExtended[Algebra[A][rank_]]:=Append[Array[{#,0}&,rank],{(rank+1)/2.,1}]
vertexCoordinatesExtended[Algebra[B][rank_]]:=Join[{{1,0.5}},Array[{#+1,0}&,rank-1],{{1,-0.5}}]
vertexCoordinatesExtended[Algebra[C][rank_]]:=Append[Array[{#+1,0}&,rank],{1,0}]
vertexCoordinatesExtended[Algebra[D][rank_]]:=Join[{{1,0.5}},Array[{#+1,0}&,rank-3],{{rank-1,0.5},{rank-1,-0.5},{1,-0.5}}]
vertexCoordinatesExtended[E6]:=Join[Array[{#,0}&,5],{{3,1},{3,2}}]
vertexCoordinatesExtended[E7]:=Join[Array[{#+1,0}&,6],{{4,1},{1,0}}]
vertexCoordinatesExtended[E8]:=Join[Array[{#,0}&,7],{{3,1},{8,0}}]
vertexCoordinatesExtended[F4]:=Append[Array[{#+1,0}&,4],{1,0}]
vertexCoordinatesExtended[G2]:={{3,0},{2,0},{1,0}}

Options[DynkinDiagram]={VertexSize->0.3};
ExtendedCartanMatrix[algebra_]:=
Module[
    {
        extendedRoot=-HighestRoot[algebra],
        extendedRow,
        extendedColumn
    },
    extendedRow=List@@extendedRoot;
    extendedColumn=Append[List@@extendedRoot,2];
    extendedCartanMatrix=MapThread[Append,{Append[CartanMatrix[algebra],extendedRow],extendedColumn}]
]

ExtendedDynkinDiagram[algebra_, opts:OptionsPattern[]]:=
Module[
    {
        vertexStyleExtended=MapIndexed[Rule[First[#2],rootColor[#1]]&,Append[Diagonal[DMatrix[algebra]],1]],
        extendedCartanMatrix=ExtendedCartanMatrix[algebra],
        edgeShapeFunctionExtended
    },
    edgeShapeFunctionExtended=Flatten[MapIndexed[edgeStyle[First[#2],Last[#2],#1]&,extendedCartanMatrix,{2}]];
    KirchhoffGraph[
        extendedCartanMatrix,
        VertexStyle->vertexStyleExtended,
        VertexSize->0.3,
        VertexLabels->vertexLabelsExtended[algebra],
        VertexCoordinates->vertexCoordinatesExtended[algebra],
        EdgeStyle->Black,
        EdgeShapeFunction->edgeShapeFunctionExtended,
        DirectedEdges->False,
        FilterRules[{opts,Options[DynkinDiagram]}, Options[KirchhoffGraph]]
    ]
]
   



(* ::Section:: *)
(* Orbits *)


   
ReflectionMatrices[algebra_?SimpleLieAlgebraQ] := ReflectionMatrices[algebra] = ReflectionMatrix /@ List @@@ OrthogonalSimpleRoots[algebra]

Reflect[weightOrRoot:(Weight|RootOmega|WeightAlpha|RootAlpha|WeightOrthogonal|RootOrthogonal)[_][__], simpleroots_] := Reflect[weightOrRoot, simpleroots] = 
    Function[root, weightOrRoot - 2 ScalarProduct[weightOrRoot, root]/ScalarProduct[root, root] root]/@ simpleroots

Reflect[weightOrRoot:(WeightOrthogonal|RootOrthogonal)[_][__]] :=  Reflect[weightOrRoot] = 
    Head[weightOrRoot] @@@  (# . List@@weightOrRoot&/@ReflectionMatrices[Algebra[weightOrRoot]])

Reflect[weights_List, simpleroots_] := DeleteDuplicates[Join[weights, Flatten[Reflect[#, simpleroots]&/@ weights,1]]]
Reflect[weights_List]               := DeleteDuplicates[Join[weights, Flatten[Reflect/@ weights,1]]]

PartialOrbit[weightOrRoot_, simpleroots_] :=
        Nest[
            Reflect[#, simpleroots]&, 
            {weightOrRoot}, 
            NumberOfPositiveRoots[Algebra[weightOrRoot]]
        ] 
        
FullOrbit[(type:(WeightOrthogonal|RootOrthogonal))[A][label__]]  := type[A] @@@ Permutations @ {label}

FullOrbit[weightOrRoot_] :=
        Nest[
            Reflect, 
            {weightOrRoot}, 
            NumberOfPositiveRoots[Algebra[weightOrRoot]]
        ]

Orbit[weightOrRoot: (Weight|RootOmega|WeightOrthogonal|RootOrthogonal)[_][__], simpleroots_] := OmegaBasis/@ PartialOrbit[OrthogonalBasis@weightOrRoot, OrthogonalBasis /@ simpleroots]

Orbit[weight:(type:(WeightOrthogonal|RootOrthogonal))[_][__]?DominantQ] := Orbit[weight] = SortBy[FullOrbit@weight, WeightLevel[#, weight]&]

Orbit[weight:(Weight|RootOmega)[_][__]?DominantQ] := Orbit[weight] = SortBy[OmegaBasis/@ FullOrbit @ OrthogonalBasis @ weight, WeightLevel[#, weight]&]

Orbit[weightOrRoot:(Weight|RootOmega|WeightOrthogonal|RootOrthogonal)[algebraClass_][__]] := 
    Module[{dominantWeightOrRoot, weightOrRootBasis=Basis[weightOrRoot]},
        If[FreeQ[DownValues[Orbit],weightOrRoot],
            dominantWeightOrRoot = 
            Seek[
	            If[algebraClass===A, 
	                weightOrRootBasis/@ FullOrbit @ OrthogonalBasis @ weightOrRoot
	            ,
	                NestWhile[
                        Reflect, {OrthogonalBasis[weightOrRoot]}, 
                        FreeQ[#,_?DominantQ]&
                    ]

	            ]
            ,DominantQ];
            Orbit[weightOrRootBasis@dominantWeightOrRoot]
        ,
            Select[DownValues[Orbit], ! FreeQ[#, weightOrRoot] &][[1, 1, 1]]
        ]
    ]

DimOrbit[args__] := DimOrbit[args] = Length[Orbit[args]]



(* ::Section:: *)
(* Irreps *)


Delta[algebra_] := Delta[algebra] = Weight[AlgebraClass[algebra]]@@ConstantArray[1, Rank[algebra]]

AddDelta[weightOrIrrep:(Weight|Irrep|RootOmega|RootAlpha)[_][label__],i_:1] := 
    Head[weightOrIrrep]@@ ({label}+i*Delta[Rank[weightOrIrrep]])

HighestWeight[irrep : Irrep[algebraClass_][__]?IrrepQ]     := Weight[algebraClass]@@irrep
HighestWeight[weight: (Weight|WeightAlpha|WeightOrthogonal)[algebraClass_][__]?DominantQ] := weight

Height[irrepOrWeight:(Irrep|Weight)[algebraClass_][irrepOrWeightLabel__]?DominantQ] := 2 Plus@@AlphaBasis@HighestWeight@irrepOrWeight

WeightLevel[weight:(Weight|WeightAlpha|WeightOrthogonal)[algebraClass_][__], irrepOrWeight:(Irrep|Weight|WeightAlpha|WeightOrthogonal)[algebraClass_][__]] := Plus@@(AlphaBasis[HighestWeight[irrepOrWeight]] - AlphaBasis[weight])

WeylDimensionFormula[algebra_] := 
WeylDimensionFormula[algebra] =
Module[
    {highestWeight, args, body, delta = Delta[algebra] },
    args = ToExpression["a" <> ToString[#]] & /@ Range[Rank[algebra]];
    highestWeight = Weight[AlgebraClass[algebra]]@@args;
    body = Factor[Times @@ (ScalarProduct[highestWeight + delta, #]/ScalarProduct[delta, #] & /@ PositiveRoots[algebra])];
    Function[Evaluate[args], Evaluate[body]]
]

Attributes[Dim] = {Listable}
Dim[ProductIrrep[irreps__]] := Dim /@ ({irreps}/.Irrep[U][_]->Sequence[])
Dim[irrep_?NonNumericIrrepQ] := Dim[irrep] = WeylDimensionFormula[Algebra[irrep]] @@ irrep
      
Attributes[Index] = {Listable}
Index[ProductIrrep[irreps__]] := Index /@ ({irreps}/.Irrep[U][_]->Sequence[])
Index[irrep:Irrep[algebraClass_][__]?NonNumericIrrepQ] :=
Index[irrep] =
    Module[ {highestWeight=Weight[algebraClass]@@irrep, delta = Delta[Algebra[irrep]]},
        Dim[irrep]/NumberOfRoots[Algebra[irrep]] ScalarProduct[highestWeight,highestWeight + 2 delta]
    ]
      
Attributes[CasimirInvariant] = {Listable}
CasimirInvariant[ProductIrrep[irreps__]] := CasimirInvariant /@ ({irreps}/.Irrep[U][_]->Sequence[])
CasimirInvariant[irrep:Irrep[algebraClass_][__]?NonNumericIrrepQ] :=
CasimirInvariant[irrep] = Index[irrep]*NumberOfRoots[Algebra[irrep]]/Dim[irrep]/2
    
GetIrrepByDim[algebra_, dim_, maxDynkinDigit_] :=  
GetIrrepByDim[algebra, dim, maxDynkinDigit] = 
    Module[{algebraClass=AlgebraClass[algebra]},
       Sort[ Select[Irrep[algebraClass] @@@ Tuples[Range[0, maxDynkinDigit], Rank[algebra]], Dim[#] == dim &], Index[#1] < Index[#2]& ]
    ]

MaxDynkinDigit[E7|E8, {label__}]                              := Max[{label}]
MaxDynkinDigit[ClassicalAlgebraTypes, {label__}]       := Max[{label}]   /; Length[{label}] >= 5
MaxDynkinDigit[ClassicalAlgebraTypes, {label__}]       := Max[{label}]+1 /; Length[{label}] <  5 
MaxDynkinDigit[E6|F4, {label__}]                              := Max[{label}]+1
MaxDynkinDigit[G2, {label__}]                                 := Max[{label}]+3
   
Attributes[DimName] = {Listable} 

DimName[irrep:Irrep[U][u1charge_]] := DimName[irrep] = u1charge

Attributes[CongruencyClass] = {Listable}
CongruencyClass[ProductIrrep[irreps__]] := CongruencyClass /@ ({irreps}/.Irrep[U][_]->Sequence[])
CongruencyClass[irrep:Irrep[A][__]?NonNumericIrrepQ] := Mod[Range[Rank[irrep]] . List@@irrep, Rank[irrep]+1]
CongruencyClass[irrep:Irrep[B][__]?NonNumericIrrepQ] := Mod[Last[List@@irrep], 2]
CongruencyClass[irrep:Irrep[C][__]?NonNumericIrrepQ] := Mod[Plus @@ Part[List @@ irrep, ;; ;; 2], 2]
CongruencyClass[irrep:Irrep[D][__]?NonNumericIrrepQ] := 
    Module[{label=List@@irrep, n=Rank[irrep]},
        CongruencyVector[Mod[Total[Take[label,-2]],2], Mod[2 Plus@@Part[Drop[label, -2], ;; ;; 2] + (n-2) Part[label, -2] + n Last[label], 4]]
    ]
CongruencyClass[irrep:Irrep[E6][__]?NonNumericIrrepQ] := Module[{a}, Evaluate[Array[a, {6}]] = List@@irrep; Mod[a[1]-a[2]+a[4]-a[5],3]]
CongruencyClass[irrep:Irrep[E7][__]?NonNumericIrrepQ] := Module[{a}, Evaluate[Array[a, {7}]] = List@@irrep; Mod[a[4]+a[6]+a[7],2]]
CongruencyClass[irrep:Irrep[E8|F4|G2][__]?NonNumericIrrepQ] := 0 

CongruencyVector /: Norm[CongruencyVector[expr__]]:=Norm[{expr}]

OrderingPart[congruencyClass_Integer] := congruencyClass
OrderingPart[congruencyClass_CongruencyVector] := Last[congruencyClass]

SortSameDimAndIndex[irreps_]:= 
    Sort[
        irreps
        ,OrderingPart[CongruencyClass[#1]] <= OrderingPart[CongruencyClass[#2]] & 
    ]
    
SimpleSO8Label[CongruencyVector[0, _]] := "v"
SimpleSO8Label[CongruencyVector[_, 0]] := "s"
SimpleSO8Label[CongruencyVector[_, _]] := "c"

NonZeroLabels[irrep_]:=Sort[DeleteDuplicates[Cases[List@@irrep, Except[0]]]]

ReducedDynkinLabel[irrep_]:=
Module[{nonZeroLabels,label=List@@irrep},
    nonZeroLabels = NonZeroLabels[irrep];
    SO8Label[Head[irrep]@@(label /. MapThread[Rule, {nonZeroLabels, Range[Length[nonZeroLabels]]}])]
]

ConcatSO8Label[irrep_] := 
    Module[{dynkinLabel = List @@ irrep, so8labels = {"v", "", "c", "s"}, nonZeroDynkinLabelOrdering},
        nonZeroDynkinLabelOrdering = Ordering[Select[dynkinLabel, Positive], All, Greater];
        StringJoin[Pick[so8labels, dynkinLabel, _?Positive][[nonZeroDynkinLabelOrdering]]]
    ]
    
SO8Label[irrep : Irrep[D][_, _, _, _]] := 
    Module[{dim = Dim[irrep],sameDimIndexCongruencyClass,sameDimIrreps, maxDynkinDigit = MaxDynkinDigit[D,List@@irrep]},
        sameDimIrreps = GetIrrepByDim[Algebra[irrep], dim, maxDynkinDigit];
        sameDimIndexCongruencyClass = Select[sameDimIrreps, Index[#] == Index[irrep] && CongruencyClass[#] == CongruencyClass[irrep] &];
        If[Length[sameDimIndexCongruencyClass] == 1,
            SimpleSO8Label[CongruencyClass[irrep]],
            If[CongruencyClass[irrep]===CongruencyVector[0,0]&&NonZeroLabels[irrep]=!={1,2},
                ReducedDynkinLabel[irrep],
                ConcatSO8Label[irrep]
            ]
        ]
    ]
    
(* Convention for SU(3) that 6 = (20) *)
DimName[Irrep[A][2,0]] = IrrepName[Algebra[A][2]][6, False, 0]
DimName[Irrep[A][0,2]] = IrrepName[Algebra[A][2]][6, True, 0]

DimName[irrep:Irrep[algebraClass_][label__]?IrrepQ] :=
DimName[irrep] =
    Module[ 
        {dim = Dim[irrep], numPrimes = 0, conjugate = 0, so8label="", 
         sameDimIrreps, maxDynkinDigit = MaxDynkinDigit[algebraClass,{label}]},
        sameDimIrreps = GetIrrepByDim[Algebra[irrep], dim, maxDynkinDigit];
        If[ Length[sameDimIrreps] > 1,
            {numPrimes,  conjugate} = First @ Position[SortSameDimAndIndex/@GatherBy[sameDimIrreps, Index], irrep] - 1;
            If[Algebra[irrep]===Algebra[D][4],
                so8label = SO8Label[irrep];
                conjugate = 0;
            ];
        ];
        If[so8label=="",
            IrrepName[Algebra[irrep]][dim, conjugate==1, numPrimes],
            IrrepName[Algebra[irrep]][dim, conjugate==1, numPrimes, so8label]
        ]
] 

DimName[ProductIrrep[irreps__]] := ProductIrrep @@ DimName /@ {irreps}

ToIrrepName[algebra_, name_] := 
    IrrepName[algebra][
        name /. Bar[dim_] :> dim /. IrrepPrime[dim_, _] :> dim,
        !FreeQ[name, Bar], 
        If[FreeQ[name, IrrepPrime], 0, name /. IrrepPrime[_, n_] :> n]
    ]
  
Irrep::noirrep := "Either an irrep with the dimension name `1` does not exist in `2` or it has at least one Dynkin digit higher than `3`. Try with $MaxDynkinDigit set to a higher value than `3`.";
  
GetIrrepByDimName[algebra_, irrepName_] := 
Module[{dim = First[irrepName], irrepWithDimName={}, maxDynkinDigit=0},
    While[irrepWithDimName==={} && maxDynkinDigit < $MaxDynkinDigit,
        maxDynkinDigit++;
	    irrepWithDimName=
	    Select[
            GetIrrepByDim[algebra, dim, maxDynkinDigit], 
            DimName[#] === irrepName &
	    ]
    ];
    If[irrepWithDimName==={}, Message[Irrep::noirrep, irrepName, algebra, $MaxDynkinDigit]];
    irrepWithDimName
]

Irrep[Algebra[U][1]][charge_] := Irrep[U][charge]

Irrep[algebra_?SimpleLieAlgebraQ][dimname_?DimNameQ] := 
Module[{irrepName=ToIrrepName[algebra, dimname], irrep}, 
    First[irrep] /; NonEmptyQ[irrep=GetIrrepByDimName[algebra,irrepName]]
]
       
SingleDominantWeightSystem[irrep:Irrep[algebraClass_][highestWeight__]?IrrepQ] :=
SingleDominantWeightSystem[irrep] =
    Module[ {},
        SortBy[Weight[algebraClass] @@@ 
            Union[
                Flatten[
                    NestWhileList[
                        Function[weights, DeleteDuplicates[Flatten[Function[weight,Select[(weight-#)&/@ToMatrix[PositiveRoots[Algebra[irrep]]], PositiveQ]]
                        /@DeleteCases[weights,{}],1]]], {{highestWeight}}, #!={}&],1]], WeightLevel[#, irrep]& ]
    ]    
    
DominantWeights[weights_] := Select[weights, DominantQ]


(* ::Subsection:: *)
(* Multiplicity *)


OmegaSimpleRoots[algebra_] := OmegaSimpleRoots[algebra] = OmegaBasis@OrthogonalSimpleRoots@algebra

T[weight_] := Position[List@@weight, 0]

Xis[algebra_, t_] := Select[PositiveRoots@algebra, PositiveQ@Extract[#, t] &]

Alphas[algebra_, t_] := Extract[OmegaSimpleRoots@algebra, t]

OrbitSizeFactor[t_, xi_] := If[EmptyQ@Complement[Position[AlphaBasis[xi], _?Positive], t], 1, 2]

XisAndMul[algebra_, t_] := XisAndMul[algebra, t] = Function[xi,{xi, OrbitSizeFactor[t, xi] DimOrbit[xi, Alphas[algebra, t]]}] /@ Xis[algebra, t]

ReflectToDominantWeight[weight_?DominantQ] := weight 
ReflectToDominantWeight[weight_] := 
    Module[{dom},
        If[FreeQ[DownValues[Orbit],weight],
            dom=OmegaBasis@
                Seek[
                    NestWhile[
                        Reflect, {OrthogonalBasis[weight]}, 
                        FreeQ[#,_?DominantQ]&
                    ]
                ,DominantQ]
        ,
            Select[DownValues[Orbit], ! FreeQ[#, weight] &][[1, 1, 1, 1]]
        ]
    ]

WeightMultiplicity[weight_?WeightQ, irrep_?IrrepQ] :=
WeightMultiplicity[weight, irrep] =
    Module[
        {   
            algebra = Algebra[irrep],
            highestWeight = HighestWeight[irrep],
            domWeight = ReflectToDominantWeight[weight],
            delta,higherWeigths            
        },
        delta = Delta[algebra];
        If[ domWeight === highestWeight,
            1,
            higherWeigths = Select[
                    Flatten[Outer[{domWeight + #1 #2[[1]], #2} &, Range[WeightLevel[weight, irrep]], XisAndMul[algebra,T[domWeight]], 1], 1], 
                    MemberQ[SingleDominantWeightSystem[irrep], ReflectToDominantWeight[#[[1]]]] &
                ];
            Plus @@ 
                (#[[2, 2]]*WeightMultiplicity[ReflectToDominantWeight[#[[1]]], irrep]*ScalarProduct[#[[1]], #[[2, 1]]] & /@ higherWeigths)/
                (ScalarProduct[highestWeight + delta, highestWeight + delta] - ScalarProduct[domWeight + delta, domWeight + delta])
        ]
    ]



(* ::Subsection:: *)
(* Weight System *)


DominantWeightSystem[irrep_] := DominantWeightSystem[irrep] = {#, WeightMultiplicity[#, irrep]}& /@ SingleDominantWeightSystem[irrep];

WeightSystemWithMul[irrep_?IrrepQ] := WeightSystemWithMul[irrep] = Flatten[(Function[weight,{weight,#2}]/@Orbit[#1]) & @@@ DominantWeightSystem[irrep],1]

Options[WeightSystem] = { SpindleShape -> False }
WeightSystem[irrep_?IrrepQ, opts:OptionsPattern[]] := 
    If[OptionValue[SpindleShape],
        Column[Row[#, " "] & /@ GatherBy[WeightSystem[irrep] , WeightLevel[#, irrep] &], ItemSize->Full, Alignment->Center]
    ,
        SortBy[ Flatten[ConstantArray[Orbit[#1], #2] & @@@ DominantWeightSystem[irrep]], WeightLevel[#, irrep]& ]
    ]
    
WeightSystem[productIrreps:ProductIrrep[__?IrrepQ]] := Tuples[ProductWeight@@(WeightSystem /@ productIrreps)]

MultiPoint[orbitMulti_] :=
 Module[{i, orbit = orbitMulti[[1]], multiplicity = orbitMulti[[2]], 
   knot},
  knot = {Disk[#, Scaled[0.012]] & /@ orbit};
  For[i = 2, i <= multiplicity, i++,
   AppendTo[knot, Circle[#, Scaled[0.012*i]] & /@ orbit];
   ];
  knot
  ]

WeightDiagram[irrep:Irrep[A][_,_]?IrrepQ]:=
Module[
    {groupedOrbits,H1=1/2 {1,0},H2=Sqrt[3]/6{1,2},rangeH1,rangeH2,labelH1,labelH2,rightEndH1Axis,upperEndH2Axis,
    arrowH1,arrowH2,axisH1,axisH2,positiveAxisFactor=1.3,negativeAxisFactor=1.2},
    groupedOrbits=Map[Dot[{H1,H2},#]&,{List@@@Orbit[#],WeightMultiplicity[#,irrep]}&/@SingleDominantWeightSystem[irrep],{3}];
    rangeH1={Min[#],Max[#]}&@Flatten[groupedOrbits[[All,1]],1][[All,1]];
    rangeH2={Min[#],Max[#]}&@Flatten[groupedOrbits[[All,1]],1][[All,2]];
    rightEndH1Axis={positiveAxisFactor*rangeH1[[2]],0};
    upperEndH2Axis={0,positiveAxisFactor*rangeH2[[2]]};
    axisH1=Line[{{negativeAxisFactor*rangeH1[[1]],0},{positiveAxisFactor*rangeH1[[2]],0}}];
    axisH2=Line[{{0,negativeAxisFactor*rangeH2[[1]]},{0,positiveAxisFactor*rangeH2[[2]]}}];
    labelH1=Text[Style["\!\(\*SubscriptBox[\"H\", \"1\"]\)",Magnification->1.5],Scaled[{-0.03,-0.05},rightEndH1Axis]];
    labelH2=Text[Style["\!\(\*SubscriptBox[\"H\", \"2\"]\)",Magnification->1.5],Scaled[{0.05,-0.03},upperEndH2Axis]];
    arrowH1={Arrowheads[0.025],Arrow[{Scaled[{0,-0.05},rightEndH1Axis],Scaled[{0.06,-0.05},rightEndH1Axis]}]};
    arrowH2={Arrowheads[0.025],Arrow[{Scaled[{0.05,0},upperEndH2Axis],Scaled[{0.05,0.06},upperEndH2Axis]}]};
    axisH1=Line[{{negativeAxisFactor*rangeH1[[1]],0},{positiveAxisFactor*rangeH1[[2]],0}}];
    axisH2=Line[{{0,negativeAxisFactor*rangeH2[[1]]},{0,positiveAxisFactor*rangeH2[[2]]}}];
    Graphics[{{PointSize->Medium,{Table[{ColorData[1][i],MultiPoint[groupedOrbits[[i]]]},{i,1,Length[groupedOrbits]}]}},{axisH1,axisH2,arrowH1,arrowH2,labelH1,labelH2}},AspectRatio->1]
]
    
    
    


(* ::Subsection:: *)
(* Young Tableau *)


Attributes[YoungTableau] = {Listable}

YoungTableau[Irrep[A][0..]] := Row[{"\[FilledSmallCircle]"}]

YoungTableau[irrep : Irrep[A][label__]?IrrepQ] :=
    Module[{maxBoxes = Length[{label}]},
        Grid[
            DeleteCases[
                Transpose@Reverse@
                Flatten[
                    MapIndexed[ConstantArray[PadRight[ConstantArray[1, Plus @@ #2], maxBoxes], #1] &, {label}]
                , 1]
            , 0, 2]
            /.Rule[1, Item[Spacer[{10, 10}], Frame -> Black]]
        , Alignment -> Right]
  ]
  
YoungTableaux[irreps_List]:=Row[YoungTableau/@irreps," "]



(* ::Section:: *)
(* Decompositions *)


(* ::Subsection:: *)
(* Branching Rules *)


(* ::Subsubsection:: *)
(* Projection Matrices *)


DropSimpleRoot[weights_,k_]:=Drop[weights,None,Flatten@{k}]

U1Charges[weights_,simpleRootToDrop_]:=Flatten@Take[List@@@AlphaBasis[weights],All,{simpleRootToDrop}]

NormalizeU1Charges[matrix_]:=MapAt[#/GCD[Sequence@@#]&,matrix,-1]

LowestOrbit[algebra_]:= Orbit[HighestWeight@GeneratingIrrep[algebra]]

NonMaximalSubalgebra[algebra_?SimpleLieAlgebraQ,simpleRootsToDrop_]:=
NonMaximalSubalgebra[algebra,simpleRootsToDrop]=
Module[{decomposedLowestOrbit,sourceLowestOrbit},
    sourceLowestOrbit=List@@@LowestOrbit[algebra];
    decomposedLowestOrbit=Drop[sourceLowestOrbit,None,simpleRootsToDrop];
    Transpose[decomposedLowestOrbit] . PseudoInverse[Transpose[sourceLowestOrbit]]
]

NonSemiSimpleSubalgebra[algebra_?SimpleLieAlgebraQ,simpleRootToDrop_]:=
NonSemiSimpleSubalgebra[algebra,simpleRootToDrop]=
Module[{decomposedLowestOrbit,sourceLowestOrbit,u1Charges,appendedU1Charges},
	sourceLowestOrbit=List@@@LowestOrbit[algebra];
	u1Charges=U1Charges[LowestOrbit[algebra],simpleRootToDrop];
	decomposedLowestOrbit=DropSimpleRoot[sourceLowestOrbit,simpleRootToDrop];
	appendedU1Charges=MapThread[Append,{decomposedLowestOrbit,u1Charges}];
	NormalizeU1Charges@Transpose[appendedU1Charges] . PseudoInverse[Transpose[sourceLowestOrbit]]
	
]

PositionMinusGamma[algebra:Algebra[A][_]|E6|F4|G2] := Rank[algebra]+1
PositionMinusGamma[algebra:Algebra[C][_]|E7] := 1
PositionMinusGamma[algebra:E8] := 8
PositionMinusGamma[algebra:Algebra[B][_]|Algebra[D][_]] := 2

ReverseOrderSimpleRoots[projectionMatrix_,range_] := Join[Reverse[Take[#, range]], Drop[#, range]] & @ projectionMatrix

ExtendedWeightScheme[algebra_?SimpleLieAlgebraQ,simpleRootToDrop_]:=
Module[{lablesWithRespectToMinusGamma,lowestOrbit=LowestOrbit[algebra],rootNumberShift = 0},
    lablesWithRespectToMinusGamma=ScalarProduct[-HighestRoot[algebra],#]&/@lowestOrbit;
    If[simpleRootToDrop<PositionMinusGamma[algebra], rootNumberShift = 1];
    Transpose[Insert[Transpose[List@@@DropSimpleRoot[lowestOrbit,simpleRootToDrop]],lablesWithRespectToMinusGamma,PositionMinusGamma[algebra]-rootNumberShift]]
]

ExtendedWeightSchemeVar[algebra_?SimpleLieAlgebraQ,simpleRootToDrop_]:=
Module[{lablesWithRespectToMinusGamma,lowestOrbit=LowestOrbit[algebra],rootNumberShift = 0},
    lablesWithRespectToMinusGamma=ScalarProduct[-HighestRoot[algebra],#]&/@lowestOrbit;
    If[simpleRootToDrop<PositionMinusGamma[algebra], rootNumberShift = 1];
    Transpose[Insert[Transpose[List@@@lowestOrbit],lablesWithRespectToMinusGamma,PositionMinusGamma[algebra]]]
]


SemiSimpleSubalgebra[algebra_?SimpleLieAlgebraQ,simpleRootToDrop_]:=
SemiSimpleSubalgebra[algebra,simpleRootToDrop]=
Module[{decomposedLowestOrbit,sourceLowestOrbit},
    sourceLowestOrbit=List@@@LowestOrbit[algebra];
    decomposedLowestOrbit=ExtendedWeightScheme[algebra,simpleRootToDrop];
    Transpose[decomposedLowestOrbit] . PseudoInverse[Transpose[sourceLowestOrbit]]
]

SpecialSubalgebra[origin_?SimpleLieAlgebraQ,targetirreps:{ProductIrrep[__?IrrepQ]..}]:=
SpecialSubalgebra[origin, targetirreps]=
 Module[{sourceIrrep,targetIrreps},
    sourceIrrep=Sort[List@@@WeightSystem[GeneratingIrrep[origin]], OrderedQ[{#2, #1}]&];
    targetIrreps=Sort[Flatten[(Flatten/@(List@@@WeightSystem[#]/.Weight[_]->List))&/@targetirreps,1], OrderedQ[{#2, #1}]&];
    Transpose[targetIrreps] . PseudoInverse[Transpose[sourceIrrep]]
]


(* ::Subsubsection:: *)
(* Sort Out Irreps *)


SortOutIrrep[sOrbits_] :=
    Module[ {i, highestIrrep,sortedOrbits=sOrbits,dominantWeights,pos},
        highestIrrep = sortedOrbits[[1,1]];
        dominantWeights = DominantWeightSystem[highestIrrep/.Weight->Irrep];
        For[i = 1,i<=Length[dominantWeights],i++,
            pos = Position[sortedOrbits[[All,1]],dominantWeights[[i,1]]][[1,1]];
            If[ sortedOrbits[[pos,2]]==dominantWeights[[i,2]],
                sortedOrbits = Delete[sortedOrbits,pos],
                sortedOrbits[[pos,2]] = sortedOrbits[[pos,2]]-dominantWeights[[i,2]];
            ];
        ];
        Sow[highestIrrep/.Weight->Irrep];
        sortedOrbits
    ]

GetIrreps[sortedOrbits_] :=
    Module[ {},
        DeleteCases[Reap[NestWhile[SortOutIrrep,sortedOrbits,#!={}&]],{}][[1,1]]
    ]
       
GetProductIrrep[decomposition_, nColumn_] :=
Module[{restColumns, mainColumn, npalgebras = Length[First[decomposition]],nRestColumns},
    nRestColumns=Delete[Range[npalgebras], nColumn];
    restColumns = decomposition[[1,nRestColumns]];
    mainColumn = decomposition[[All,nColumn]];
    Insert[restColumns, #, nColumn]&/@ GetIrreps[DominantWeightsAndMul@mainColumn]
]    

GetAllProductIrrep[alldecomposition_, nColumn_] :=
Module[{npalgebras = Length[First[alldecomposition]],nRestColumns},
    nRestColumns=Delete[Range[npalgebras], nColumn];
    Flatten[GetProductIrrep[#, nColumn]& /@ GatherBy[alldecomposition,#[[nRestColumns]]&],1]
]



(* ::Subsubsection:: *)
(* Irrep Decomposition *)


UnequalPartition[list_, {n_Integer}] := {Take[list, n]}
UnequalPartition[list_, n_List] := Join[{Take[list, First[n]]}, UnequalPartition[Drop[list, First[n]], Rest[n]]]

Attributes[DecomposeIrrep] = {Listable}

IrrepPlus  /: DecomposeIrrep[IrrepPlus[irreps__],algebra_?LieAlgebraQ, args___] := Apply[IrrepPlus,DecomposeIrrep[{irreps},algebra,args],{0,1}]
IrrepTimes /: DecomposeIrrep[IrrepTimes[factor_?IntegerQ, irrep_?SingleOrProductIrrepQ], algebra_?LieAlgebraQ, pos_] := IrrepTimes[factor,DecomposeIrrep[irrep,algebra,pos]]

DecomposeIrrep[irrep_?IrrepQ, algebra_?SimpleLieAlgebraQ, args___] := DecomposeIrrep[irrep, ProductAlgebra[algebra], args]/. ProductIrrep->Sequence

Project[matrix_, weights_] := matrix . # & /@ List @@@ weights

DecomposeIrrep::nosubalgebra := "Either `1` does not have `2` as a subalgebra or the branching rule is implemented with a different ordering of `2` or not at all. Try changing the order in `2`.";

GroupProjectedWeights[projectedWeights_,target_] :=
    Module[{irrepheads},
        irrepheads=Weight/@AlgebraClass@target;
        MapThread[Apply, {irrepheads, #}] & /@ (UnequalPartition[#, Rank[List@@target]]& /@ projectedWeights)
    ] 

DecomposeIrrep[irrep_?IrrepQ, target_?ProductLieAlgebraQ, branchingIndex_ | PatternSequence[]] :=
Module[ {irrepsOfAll, dominantProjectedWeights, cutU1Charge},
        cutU1Charge = If[Last[target]===Algebra[U][1], Most, Identity]; 
        dominantProjectedWeights = Select[Project[ProjectionMatrix[Algebra[irrep], target, branchingIndex],WeightSystem[irrep]], PositiveQ[cutU1Charge[#]]&];
        irrepsOfAll=Fold[GetAllProductIrrep,GroupProjectedWeights[dominantProjectedWeights,target],Range@Length@cutU1Charge@target];
        IrrepPlus[ProductIrrep @@@ irrepsOfAll/.Weight[U]->Irrep[U]]
] /; If[Head[ProjectionMatrix[Algebra[irrep], target, branchingIndex]]===List, True, Message[DecomposeIrrep::nosubalgebra, Algebra[irrep], target]; False]

Attributes[ProductIrrep] = {Flat, OneIdentity}

DecomposeIrrep[irrep_?ProductIrrepQ, algebra_?SimpleLieAlgebraQ, args___] := DecomposeIrrep[irrep, ProductAlgebra[algebra], args]

DecomposeIrrep[productIrrep_?ProductIrrepQ, target_?ProductLieAlgebraQ, pos_, branchingIndex_ | PatternSequence[]] :=
Module[{decomposition},
    decomposition = MapAt[IrrepList[DecomposeIrrep[#, target, branchingIndex]] &, productIrrep, pos];
    IrrepPlus[Sort[#, OrderedQ[{First@Head[#1], First@Head[#2]}] &] & /@ Thread[decomposition]]
]


(* ::Subsection:: *)
(* Fast Tensor Product Decomposition for SU(N) *)


AddToTableau[irrep_, rowcombinations_] :=
 Module[ {n = Length[irrep[[1]]], added},
     added = (irrep[[1]] + Plus @@ (PadRight[PadLeft[{-1, 1}, #], n] & /@ #)) & /@ rowcombinations;
     MapThread[List, {added, rowcombinations}]
 ]

BoxesToBump[irrep_, row_] := Total[Drop[irrep, row - 1]]

PathCounts[comb_, maxn_] := Count[comb, x_ /; x <= #] & /@ Range[maxn]

AllowedCombination[comb1_, comb2_] :=
    Module[ {maxn = Max[comb1, comb2]},
        If[ comb1 == {},
            True,
            And @@ Thread[
                LessEqual[
                    PadRight[PathCounts[comb2, maxn], maxn + 1],
                    PadLeft[PathCounts[comb1, maxn], maxn + 1]
                ]
            ]
        ]
    ]

AllowedRows[irrep_, nboxes_] :=
    Module[ {result},
        result = Select[
          Union[Subsets[
            Join[ConstantArray[1, If[ irrep[[2]] == {},
                                      nboxes,
                                      0
                                  ]],
             Flatten[DeleteCases[
               ConstantArray[# + 1,
                  If[ irrep[[2]] == {},
                      Min[irrep[[1, #]], nboxes],
                      Min[Count[irrep[[2]], x_ /; x <= #], irrep[[1, #]],
                       nboxes]
                  ]] & /@ Range[Length[irrep[[1]]]], {}],
              1]], {nboxes}]], AllowedCombination[irrep[[2]], #] &];
        result
    ]

NumberOfBoxes[irrep_] := Module[{n = Rank[irrep]},
    Total[MapThread[Times, {Range[n], List @@ irrep}]]
]

NumberOfBoxesOrderedQ[irrep_,irrep_]:=True
NumberOfBoxesOrderedQ[irrep1_,irrep2_]:=Dim[irrep1]>=Dim[irrep2]/;NumberOfBoxes[irrep1]==NumberOfBoxes[irrep2]&&Dim[irrep1]!=Dim[irrep2]
NumberOfBoxesOrderedQ[irrep1_,irrep2_]:=Index[irrep1]<=Index[irrep2]/;NumberOfBoxes[irrep1]==NumberOfBoxes[irrep2]&&Dim[irrep1]==Dim[irrep2]&&Index[irrep1]!=Index[irrep2]
NumberOfBoxesOrderedQ[irrep1:Irrep[A][__],irrep2:Irrep[A][__]] := NumberOfBoxes[irrep1]>=NumberOfBoxes[irrep2]

DecomposeProduct[irreps:Irrep[A][__?DynkinLabelQ]..] /; !OrderedQ[{irreps},NumberOfBoxesOrderedQ] := DecomposeProduct[Sequence@@Sort[{irreps}, NumberOfBoxesOrderedQ]]
    
DecomposeProduct[irrep1:Irrep[A][label1__?DynkinLabelQ], irrep2:Irrep[A][label2__?DynkinLabelQ]] := 
(DecomposeProduct[irrep1, irrep2] =
    Module[{},
         IrrepPlus[Irrep[A] @@@ Fold[Flatten[Map[Function[label, AddToTableau[label, AllowedRows[label, BoxesToBump[{label2}, #2]]]], #1], 1] &, {{{label1}, {}}}, Range[Rank[irrep2]]][[All, 1]]]
    ] 
    ) /; Algebra[irrep1] === Algebra[irrep2]



(* ::Subsection:: *)
(* Irrep Product Decomposition *)


Attributes[DecomposeProduct] = {Listable}

IrrepPlus  /: DecomposeProduct[irreps1_IrrepPlus,irreps2_IrrepPlus] := IrrepPlus @@ DecomposeProduct @@@ Tuples[{irreps1, irreps2}]
IrrepPlus  /: DecomposeProduct[irreps1_IrrepPlus,irrep2__?SingleOrProductIrrepQ] := IrrepPlus @@ (DecomposeProduct[#,irrep2]&/@ (List@@irreps1))
IrrepPlus  /: DecomposeProduct[irrep1__?SingleOrProductIrrepQ,irreps2_IrrepPlus] := IrrepPlus @@ (DecomposeProduct[irrep1,#]&/@ (List@@irreps2))
IrrepTimes /: DecomposeProduct[IrrepTimes[factor1_?NaturalNumberQ, irrep1_?SingleOrProductIrrepQ], IrrepTimes[factor2_?NaturalNumberQ, irrep2_?SingleOrProductIrrepQ]] := IrrepTimes[factor1*factor2,DecomposeProduct[irrep1,irrep2]]
IrrepTimes /: DecomposeProduct[irrep1__?SingleOrProductIrrepQ, IrrepTimes[factor_?NaturalNumberQ, irrep2_?SingleOrProductIrrepQ]] := IrrepTimes[factor,DecomposeProduct[irrep1,irrep2]]
IrrepTimes /: DecomposeProduct[IrrepTimes[factor_?NaturalNumberQ, irrep1_?SingleOrProductIrrepQ], irrep2__?SingleOrProductIrrepQ] := IrrepTimes[factor,DecomposeProduct[irrep1,irrep2]]
   
DecomposeProduct[irrep_?SingleOrProductIrrepQ] := {irrep}

DecomposeProduct[u1Irreps__?U1ChargeQ] := {Irrep[U] @@ Plus @@ List @@@ {u1Irreps}}

DecomposeProduct[irreps:Irrep[Except[A]][__]?IrrepQ..] /; !OrderedQ[{irreps},IrrepOrderedQ] := (DecomposeProduct[Sequence@@Sort[{irreps}, IrrepOrderedQ]])

DominantWeightsAndMul[weights_] := SortBy[Tally[Select[weights, DominantQ]], -Height[First[#]]&]
    
DimOrderedQ[irrep_,irrep_]:=True
DimOrderedQ[irrep1_,irrep2_]:=Dim[irrep1]<=Dim[irrep2]/;Dim[irrep1]!=Dim[irrep2]

DecomposeProduct[irreps:Irrep[Except[A]][__]?IrrepQ] /; !OrderedQ[{irreps}, DimOrderedQ] := DecomposeProduct[Sequence@@Sort[{irreps}, DimOrderedQ]]
    
TrivialStabilizerWeights[weights_]:=Select[weights,FreeQ[First[#],0]&]

ToIrrep[highestWeight_]:=highestWeight/.Weight->Irrep
    
ReflectToDominantWeightWithMul[{weight_?DominantQ,mul_}] := {weight,mul}

ReflectToDominantWeightWithMul[{weight_,mul_}] := 
    Module[{dom,numrefl=0},
            dom={OmegaBasis@
                Seek[
                    NestWhile[
                        (numrefl++; Reflect[#])&, {OrthogonalBasis[weight]}, 
                        FreeQ[#,_?DominantQ]&
                    ]
                ,DominantQ],mul*(-1)^numrefl}
    ]   

Add[talliedWeights_,weight_]:= MapAt[#+weight&,#,1]&/@talliedWeights

DecomposeProduct[irrep1:Irrep[_][__]?IrrepQ,irrep2:Irrep[_][__]?IrrepQ]:=
(DecomposeProduct[irrep1, irrep2] =
	Module[{delta=Delta[Algebra[irrep1]],mu,irreps},
	    mu=Add[WeightSystemWithMul[irrep1],HighestWeight[irrep2]+delta];
	    irreps=ToIrrep/@Add[TrivialStabilizerWeights[ReflectToDominantWeightWithMul/@mu],-delta];
	    IrrepPlus@@(IrrepTimes[#2,#1]&@@@Sort[Select[{#[[1,1]],Total[#[[All,2]]]}&/@GatherBy[irreps,#[[1]]&],Positive@Last@#&],IrrepOrderedQ[First@#1, First@#2]&])
	]
) /; Algebra[irrep1] === Algebra[irrep2]

DecomposeProduct[irrep1_?SingleOrProductIrrepQ, irrep2_?SingleOrProductIrrepQ, irreps__?SingleOrProductIrrepQ] :=
    DecomposeProduct[DecomposeProduct[irrep1, irrep2], irreps] /; SameQ[Algebra[irrep1], Algebra[irrep2], Sequence@@Algebra@{irreps}]
    
DecomposeProduct[HoldPattern[productIrreps__?ProductIrrepQ]] :=
    IrrepPlus@Flatten@Outer[ProductIrrep, Sequence @@ (IrrepList /@ DecomposeProduct @@@ Transpose[List @@@ {productIrreps}])]
    
SingletIrrepPattern = Irrep[_][0..]
SingletProductIrrepPattern = ProductIrrep[Irrep[_][0..]..]

ContainsSingletQ[irreps_List] := ContainsSingletQ[irreps] = MemberQ[irreps, SingletIrrepPattern] || MemberQ[irreps, SingletProductIrrepPattern]
ContainsSingletQ[irreps_IrrepPlus] := ContainsSingletQ[IrrepList@irreps] 
SingletInDecompositionQ[irreps__]:=SingletInDecompositionQ[irreps]=ContainsSingletQ@DecomposeProduct[irreps]

End[]
EndPackage[]
