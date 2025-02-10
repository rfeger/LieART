(* ::Package:: *)
(* :Title: LieART`Tables *)
(* :Authors: Robert Feger, Thomas Kephart and  Robert Saskowski *)
(* :Summary: *)
(* :Context:   LieART`Tables` *)
(* :Package version:   2.1.1 *)
(* :Copyright:  Copyright 2012-2025, Robert Feger, Thomas Kephart and Robert Saskowski. *)

(*This file is part of LieART. See LieART.m for licensing information*) 

BeginPackage["LieART`Tables`", { "LieART`","LieART`BranchingRules`"}]

Starred::usage = "Starred  "
StarredVar::usage = "StarredVar  "
MaxDim::usage="MaxDim";
MaxDynkinDigit::usage="MaxDynkinDigit";
MaxDynkinDigitInResult::usage="MaxDynkinDigitInResult";
CongruencyClassName::usage="CongruencyClassName";
CongruencyClasses::usage="CongruencyClasses";
TableCaption::usage="TableCaption"

$IrrepPropertiesLaTeXFile::usage = "$IrrepPropertiesLaTeXFile  "
$TensorProductsLaTeXFile::usage = "$TensorProductsLaTeXFile  "
$BranchingRulesLaTeXFile::usage = "$BranchingRulesLaTeXFile  "

$WriteToLaTeXFile::usage = "$WriteToLaTeXFile  "

IrrepPropertiesTable::usage = "IrrepPropertiesTable  "
TensorProductsTable::usage = "TensorProductsTable  "
BranchingRulesTable::usage = "BranchingRulesTable  "

MaxDimInResult::usage = "MaxDimInResult  ";
BasicIrrepsOnly::usage = "BasicIrrepsOnly  ";

Begin["`Private`"] 

(* ::Section:: *)
(* Initial Settings *)

tableNumber=0;
$WriteToLaTeXFile=False;
$IrrepPropertiesLaTeXFile="IrrepPropertiesTables.tex";
$TensorProductsLaTeXFile="TensorProductsTables.tex";
$BranchingRulesLaTeXFile="BranchingRulesTables.tex";

(* ::Section:: *)
(* Utility Functions *)

Attributes[Add]=HoldAll;
Add[numbers_,pos_,max_]:=
Module[{},
    If[numbers[[pos]]==max&&pos==1,
        numbers=ConstantArray[0,Length[numbers]]
        ,
        If[numbers[[pos]]==max,
            numbers[[pos]]=0;
            Add[numbers,pos-1,max]
            ,
            numbers[[pos]]=numbers[[pos]]+1
        ];
    ];
]

Options[IrrepsWithMaxDim]={ConjugateIrreps->False};

IrrepsWithMaxDim[algebra_,maxdim_,maxDynkinDigit_,opts:OptionsPattern[]]:=
IrrepsWithMaxDim[algebra,maxdim,maxDynkinDigit,opts]=
Module[{i,n=Rank[algebra],algebraClass=AlgebraClass[algebra],dynkinLabel,irreps},
    dynkinLabel=ConstantArray[0,n];
    irreps=Reap[
        For[i=1,i<(maxDynkinDigit+1)^n,i++;
            Add[dynkinLabel,n,maxDynkinDigit];
            If[Dim[Irrep[algebraClass]@@dynkinLabel]<=maxdim,
                If[OptionValue[ConjugateIrreps]||DimName[Irrep[algebraClass]@@dynkinLabel][[2]]==False,
                    Sow[Irrep[algebraClass]@@dynkinLabel];
                ];
            ];
        ];
    ][[2,1]];
    Sort[irreps,IrrepOrderedQ[#1,#2]&]
]

ClearDimDownValues:=Cases[DownValues[Dim],dimDownValue:HoldPattern[Dim[Irrep[_][__]]]:>Unset[dimDownValue],3];
ClearSavedIrrepProperties[expr__]:=
    Module[{},
        Cases[DownValues[#],downValue:HoldPattern[#[Irrep[_][__]]]:>Unset[downValue],3]&/@Flatten[{expr}];
        (* Convention for SU(3) that 6 = (20) *)
        DimName[Irrep[A][2,0]] = IrrepName[Algebra[A][2]][6, False, 0];
        DimName[Irrep[A][0,2]] = IrrepName[Algebra[A][2]][6, True, 0];
    ]

(* ::Section:: *)
(* Formatting *)

Format[Starred[singletCount_],TraditionalForm]:=Superscript[singletCount,"\[SixPointedStar]"]
Format[StarredVar[singletCount_,U1singletCount_],TraditionalForm]:=Row[{singletCount,"+",Superscript[U1singletCount,"\[SixPointedStar]"]}]
Format[Starred[singletCount_],LaTeXForm]:="\\starred{"<>ToString[singletCount]<>"}"
Format[StarredVar[singletCount_,U1singletCount_],LaTeXForm]:=ToString[singletCount]<>"+\\starred{"<>ToString[U1singletCount]<>"}"

Format[AlignPrime[irrepname:IrrepName[algebra_][dim_, conjugated_, numPrimes_, subscript_]],TraditionalForm]:=
        DisplayForm@Style[RowBox[{dim,"\[InvisibleSpace]",SubsuperscriptBox["\[AlignmentMarker]",subscript,StringJoin[ConstantArray["\[Prime]", numPrimes]]]}],Bold]
        
Format[AlignPrime[irrepname:IrrepName[algebra_][dim_, conjugated_, numPrimes_]],TraditionalForm]:=
        DisplayForm@Style[RowBox[{dim,"\[InvisibleSpace]",SuperscriptBox["\[AlignmentMarker]",StringJoin[ConstantArray["\[Prime]", numPrimes]]]}],Bold]

Format[AlignPrime[irrepname_],LaTeXForm]:= ToLaTeX[irrepname]

(* ::Subsection:: *)
(* Irrep Properties Table *)

Format[DisplayIrrepPropertiesTable[algebra_,data_,indexDivisor_,subalgebras_,congruencyClassName_],TraditionalForm]:=
Module[{table,tableCaption=ToString[algebra,TraditionalForm]<>" Irreps",semisimplesubalgebras=DeleteCases[#,U1]&/@subalgebras},
    table={Grid[Join[
            {{
                Column[{"Dynkin","label"}],
                Column[{"Dimension","(name)"}],
                Column[{Row[{Style["l",Italic],If[indexDivisor>1,"/"<>ToString[indexDivisor],""]}],"(index)"}],
                If[congruencyClassName==={},Column[{"Congruency","class"}],Column[Flatten[{congruencyClassName}]]],
                Sequence@@(Column[{#,"singlets"}]&/@semisimplesubalgebras)
            }},
            data]
            ,Background->{None,{Lighter[Gray,.6],{White,Lighter[Gray,.8]}}}
            ,Dividers->{{White,{False},White},{True,True,{False},True}}
            ,Alignment->{{Left,"\[AlignmentMarker]",Right,{Left}},{Bottom,{Center}}}
            ,Spacings->{{2,1,1,3,{2}},{1,1,{Automatic},0.5}}
            ,ItemSize->Full
        ]};
    Grid[Prepend[
            If[subalgebras=!={}&&!FreeQ[subalgebras,U1],
                Append[{table},{Row[{Superscript["","\[SixPointedStar]"],Row[Cases[subalgebras,ProductAlgebra[__,U1]]," and "],If[Length[subalgebras]==1," singlet."," singlets resp."]}]}]
            ,
                {table}
            ],
        {Row[{Style[Row[{"Table\[MediumSpace]",tableNumber,": "}],Bold],tableCaption}]}
        ]
    ,
    Alignment->{Left}]
]

Format[DisplayIrrepPropertiesTable[algebra_,data_,indexDivisor_,subalgebras_,congruencyClassName_],LaTeXForm]:=
Module[{head,foot,tableCaption=ToString[ToLaTeX[algebra]<>" Irreps"],semisimplesubgroups=DeleteCases[#,U1]&/@subalgebras},
    head= StringJoin[
        "\\toprule\n",
        "\\rowcolor{tableheadcolor}\n",
        "Dynkin & Dimension & l",If[indexDivisor>1,"/"<>ToString[indexDivisor],""]," & ",If[congruencyClassName==={},"Congruency",""],If[subalgebras=!={}," & "<>ToString[Row[ToLaTeX/@semisimplesubgroups," & "]],""],"\\\\\n",
        "\\rowcolor{tableheadcolor}",
        "label & ",
        "(name)",
        " & (index) & ",
        If[congruencyClassName==={},"class",ToString[congruencyClassName]],
        If[subalgebras=!={}," & "<>ToString[Row[ConstantArray["singlets",Length[subalgebras]]," & "]],""]
        ,"\\\\\n",
        "\\midrule \n"
        ];
    foot= If[subalgebras=!={}&&!FreeQ[subalgebras,U1],StringJoin["\\multicolumn{",ToString[4+Length[subalgebras]],"}{l}{\\footnotesize $^\\ast$" ,ToString[Row[ToLaTeX/@Cases[subalgebras,ProductAlgebra[__,U1]]," and "]],
             If[Length[subalgebras]==1," singlet.}\n"," singlets resp.}\n"]],""];
     StringJoin[
        "\\begin{longtable}{lrrc",ToString[Row[ConstantArray["c",Length[subalgebras]]]],"}\n", 
        
        "\\caption{\\label{tab:",StringReplace[tableCaption,{" "->"","("->"",")"->"","\\"->""}],"}",tableCaption,"}\\\\\n",
        head,
        "\\endfirsthead\n",
        "\\caption[]{",tableCaption," (continued)}\\\\\n",
        head,
        "\\endhead\n",
        foot,
        "\\endfoot\n",
        "\\bottomrule\n",
        foot,
        "\\endlastfoot\n",   
        StringJoin[ StringJoin[#,"\\\\\n"]&/@(ToString[Row[ToLaTeX/@#," & "]]&/@data)] ,
        "\\end{longtable}\n",
        "\\newpage\n"
    ]
]

(* ::Subsection:: *)
(* Tensor Products Table *)

Format[DisplayTensorProductsTable[algebra_,productDecompositions_],TraditionalForm]:=
Module[{tableCaption=Row[{algebra," Tensor Products"}]},
    Grid[Prepend[
        {{Grid[
            Riffle[#," = "]&/@productDecompositions
            ,Dividers->{{White,{False},White},{True,{False},True}}
            ,Background->{None,{{White,Lighter[Gray,.8]}}}
            ,Alignment->{{Right,Center,Left},{Bottom,{Center}}}
            ,Spacings->{{2,0.5,0.5,2},{Automatic}}  
        ]}},
        {Row[{Style[Row[{"Table\[MediumSpace]",tableNumber,": "}],Bold],tableCaption}]}
    ],
    Alignment->{Left}]
]

Format[DisplayTensorProductsTable[algebra_,productDecompositions_],LaTeXForm]:=
Module[{tableCaption=ToString[ToLaTeX[algebra]<>" Tensor Products"]},
     StringJoin[
        "\\begin{longtable}{rcl}\n", 
        "\\caption{\\label{tab:",StringReplace[tableCaption,{" "->"","("->"",")"->"","\\"->""}],"}",tableCaption,"}\\\\\n",
        "\\toprule\n",
        "\\endfirsthead\n",
        "\\caption[]{",tableCaption," (continued)}\\\\\n",
        "\\endhead\n",
        "\\bottomrule\n",
         "\\endlastfoot\n",
        StringJoin[ToString[Row[Riffle[ToLaTeX/@#," & = & "]]],"\\\\\n"]&/@productDecompositions,
        "\\end{longtable}\n",
        "\\newpage\n"
    ]
]

(* ::Subsection:: *)
(* Branching Rules Table *)

Format[AlgebraBranching,TraditionalForm]:="\[RightArrow]"
Format[AlgebraBranching,LaTeXForm]:="& $\\to$ &"
Format[IrrepBranching,TraditionalForm]:="="
Format[IrrepBranching,LaTeXForm]:="="

Format[DisplayBranchingRulesTable[algebra_,branchingRules_],TraditionalForm]:=
Module[{gridBranchingRules,tableCaption=Row[{algebra," Branching Rules"}]},
    gridBranchingRules=Grid[{
                {Grid[
                    #
                    ,Dividers->{{White,{False},White},{True,True,{False},True}}
                    ,Background->{None,{Lighter[Gray,.6],{White,Lighter[Gray,.8]}}}
                    ,Alignment->{{"\[AlignmentMarker]",Center,Left},{Center,{Center}}}
                    ,Spacings->{{1,0.25,0.5,2},{1,1,{Automatic},0.5}}
                ]}
                },
                Alignment->{Left,Automatic}, ItemSize->Full,Spacings->{0,0}
            ]&/@branchingRules;
    Column[
        Prepend[gridBranchingRules,Row[{Style[Row[{"Table\[MediumSpace]",tableNumber,": "}],Bold],tableCaption}]],
        Alignment->{Left,Automatic},
        Spacings->{Automatic,{0,0.6,{1},0}}, ItemSize->Full
    ]
]

Format[DisplayBranchingRulesTable[algebra_,branchingRules_],LaTeXForm]:=
Module[{tableCaption=ToString[ToLaTeX[algebra]<>" Branching Rules"]},
     StringJoin[
        "\\begin{longtable}{rcp{0.9\\textwidth}}\n", 
        "\\caption{\\label{tab:",StringReplace[tableCaption,{" "->"","("->"",")"->"","\\"->""}],"}",tableCaption,"}\\\\\n",
        "\\endfirsthead\n",
        "\\caption[]{",tableCaption," (continued)}\\\\\n",
        "\\endhead\n",
        "\\endfoot\n",
        Riffle[
            Function[{table},
                StringJoin[ 
                    StringJoin["\\toprule\n","\\rowcolor{tableheadcolor}\n",ToString[StringJoin[ToLaTeX/@table[[1]]]],"\\\\\n\\midrule \n"],
                    StringJoin[#,"\\\\\n"]&/@(ToString[Row[ToLaTeX/@#," & "]]&/@table[[2;;]]),
                    "\\bottomrule\n"
                ]
            ]/@branchingRules,
        "\\rowcolor{white}\\\\[-\\medskipamount]\n"
        ] ,
        "\\end{longtable}\n",
        "\\newpage\n"
    ]
]


(* ::Section:: *)
(* Table Generation *)


(* ::Subsection:: *)
(* Irrep Properties Table *)

SingletInDecomposition[irrep_,subgroup_]:=
Module[
    {singletCount,U1singletCount,decomposition},
    decomposition=IrrepList[DecomposeIrrep[irrep,If[FreeQ[subgroup,ProductAlgebra],ProductAlgebra[subgroup],subgroup]]];
    If[FreeQ[subgroup,U1],
        singletCount=Count[decomposition,ProductIrrep[Irrep[_][0..]..]]
        ,
        singletCount=Count[decomposition,ProductIrrep[Irrep[_][0..]..,Irrep[U][Except[0]]]];
        U1singletCount=Count[decomposition,ProductIrrep[Irrep[_][0..]..,Irrep[U][0]]];
	    If[U1singletCount>0,
	        If[singletCount>0,
	            StarredVar[singletCount,U1singletCount],
                Starred[U1singletCount]
	        ]
        ,
            singletCount
        ]
    ]
]

Options[IrrepPropertiesTable]={MaxDim->100,MaxDynkinDigit->3,CongruencyClassName->{}};
IrrepPropertiesTable[algebra_,subalgebras_List,opts:OptionsPattern[]]:=
Module[
    {irreps,data,indexDivisor,time,file},
    ClearSavedIrrepProperties[Dim,DimName,Index];
    time = First@AbsoluteTiming[
        tableNumber++;
        irreps=IrrepsWithMaxDim[algebra,OptionValue[MaxDim] ,OptionValue[MaxDynkinDigit],ConjugateIrreps->False];
        Print["Number of irreps: ",Length[irreps]];
        indexDivisor=Index[First[irreps]];
        data=  {
            DynkinLabel[#],
            AlignPrime[DimName[#]],
            Index[#]/indexDivisor,
            CongruencyClass[#],
            Sequence@@(Function[subgroup,SingletInDecomposition[#,subgroup]]/@subalgebras)
            }&/@irreps;
    ];
    Print["Total time: ",NumberForm[time,{3,1}],"s"];
    If[$WriteToLaTeXFile,
        file=OpenAppend[$IrrepPropertiesLaTeXFile,PageWidth->Infinity];
        Write[file,LaTeXForm[DisplayIrrepPropertiesTable[algebra,data,indexDivisor,subalgebras,OptionValue[CongruencyClassName]]]];
        Close[file];
    ];
    DisplayIrrepPropertiesTable[algebra,data,indexDivisor,subalgebras,OptionValue[CongruencyClassName]]
]

(* ::Subsection:: *)
(* Tensor Products Table *)

Options[TensorProductsTable]={MaxDim->100,MaxDynkinDigit->3,MaxDynkinDigitInResult->3,MaxDimInResult->200,BasicIrrepsOnly->False};
TensorProductsTable[algebra_,opts:OptionsPattern[]]:=
Module[{irreps,irrepPairs,productDecompositions,time,file},
    ClearSavedIrrepProperties[Dim,DimName,Index];
    time = First@AbsoluteTiming[
        tableNumber++;
        irreps= 
            If[OptionValue[BasicIrrepsOnly],
                BasicIrreps[algebra],
                IrrepsWithMaxDim[algebra,OptionValue[MaxDim],OptionValue[MaxDynkinDigit],ConjugateIrreps->True]
            ];
        irrepPairs=SortBy[
                DeleteDuplicates[
                    Sort[#,!IrrepOrderedQ[#1,#2]&]&/@
                        Tuples[{irreps,irreps}]                      
                ,#1===#2||Bar[#1]===#2&
            ],
            Dim[#[[1]]]&&Dim[#[[2]]]&
        ];
        productDecompositions=Select[
            {IrrepProduct@@#,DecomposeProduct[Sequence@@#]}&/@irrepPairs,
            FreeQ[Cases[Last[#], irrep : Irrep[_][__] :> Dim[irrep], 2],d_/;d>OptionValue[MaxDimInResult]]&
        ];
        Print["Number of Tensor Products: ",Length[productDecompositions]];
    ];
    Print["Total time: ",NumberForm[time,{3,1}],"s"];
    If[$WriteToLaTeXFile,
        file=OpenAppend[$TensorProductsLaTeXFile,PageWidth->Infinity];
        Write[file,LaTeXForm[DisplayTensorProductsTable[algebra,productDecompositions]]];
        Close[file];
    ];
    DisplayTensorProductsTable[algebra,productDecompositions]
]

(* ::Subsection:: *)
(* Branching Rule Table *)

BranchingRules[irreps_,SU15,{SU3}]:=
	Join@@(BranchingRules[irreps, SU15,{SU3},#]&/@{1,2})
	
BranchingRules[irreps_,E7,{SU2}]:=
	Join@@(BranchingRules[irreps, E7,{SU2},#]&/@{1,2})
	
BranchingRules[irreps_,E8,{SU2}]:=
	Join@@(BranchingRules[irreps, E8,{SU2},#]&/@{1,2,3})
		
Options[BranchingRules]={MaxDim->100,MaxDynkinDigit->3};
BranchingRules[irreps_,algebra_,subalgebras_,index_|PatternSequence[]]:=
	Function[subgroup,
	    Join[
	        {{algebra,AlgebraBranching,subgroup}},
	        {AlignPrime[DimName[#]],IrrepBranching,DecomposeIrrep[#,subgroup,index]}&/@irreps
	    ]
	]/@subalgebras
	
	
Options[BranchingRulesTable]={MaxDim->100,MaxDynkinDigit->3};
BranchingRulesTable[algebra_,subalgebras_,index_|PatternSequence[], opts:OptionsPattern[]]:=
Module[{irreps,branchingRules,time,file},
    ClearSavedIrrepProperties[Dim,DimName,Index];
    time = First@AbsoluteTiming[
        tableNumber++;
        irreps=IrrepsWithMaxDim[algebra,OptionValue[MaxDim] ,OptionValue[MaxDynkinDigit]];
        branchingRules=BranchingRules[irreps,algebra,subalgebras,index];
    ];
    Print["Total time: ",NumberForm[time,{3,1}],"s"];
    If[$WriteToLaTeXFile,
        file=OpenAppend[$BranchingRulesLaTeXFile,PageWidth->Infinity];
        Write[file,LaTeXForm[DisplayBranchingRulesTable[algebra,branchingRules]]];
        Close[file];
    ];
    DisplayBranchingRulesTable[algebra,branchingRules]
]


End[]

EndPackage[]
