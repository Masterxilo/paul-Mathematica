(* ::Package:: *)

(*:Name: paul` *)

(*:Author: Paul Frischknecht *)

(*:Context: paul` *)

(*:Package Version: work-in-progress 1.1 *)

(*:License: WTFPL *)

(*:History:
   Paul Frischknecht, 2016
   1.0
   1.1 replaced IterateWith with LetL
*)

(* :Warning:

* This package Makes ArrayReshape work with any expression, despite this generting
{ArrayReshape::list, Part::partd, Part::partd, Part::partd,
 General::stop}

* This package adds the attribute HoldFirst to HoldFirst to make it act like an anonymous data-structure with this property.

*)

(* :Summary: Unsorted collection of various utilities *)

(* :Source:
  TraceView, TraceViewFlat by WReach (traceView2, traceView4)
    http://mathematica.stackexchange.com/a/29340/6804
  BROKEN! IterateWith modified from
    https://www.wolfram.com/mathematica/new-in-10/inactive-objects/transform-code.html
  replaced with LetL by Leonid Shifrin from http://mathematica.stackexchange.com/a/10451/6804
  *)

(* :Bugs:
---
  This creates an error when run after <<paul` has been loaded:

  f = 0 &;
DensityPlot3D[f[x, y, z], {x, 1, 3}, {y, 1, 3}, {z, 1, 3}]

ClearAll["paul`*", "paul`Private`*"] does not seem to resolve the issue, only restarting the Kernel without <<paul` does.

It happens even when we don't modify the built-in ArrayReshape.
--- > this was caused by setting $PrePrint
*)

BeginPackage["paul`",  {"BoolEval`"}];

ClearAll@"paul`*";

AllEqual::usage = "AllEqual[list, property] check whether applying property to each element of list
yields elements which are Equal.
Equal@@(property  /@ list)

AllEqual[property] Operator form"

FirstNonNull::usage = "FirstNonNull[args__] evaluates each argument in turn,
returns the first of its arguments that
does not evaluate to Null (similar to how And stops with the first True or Which
only executes tests until they are true)"

ImportObjVC::usage="ImportObjVC[file] Imports v (6 component xyzrgb) and f data from an obj file and constructs a GraphicsComplex. Handles obj files with v and f directives only";

RotationMatrixAxisAngleVector::usage =
"RotationMatrixAxisAngleVector[w] is RotationMatrix[Norm@w, Normalize@w]"

ColumnVector::usage =
"ColumnVector[v_] Converts v to a column vector if necessary
and throws an assertion error if this is not possible."

EnsureList::usage = "EnsureList[x] if x is not a list, wraps it in one"


PartitionIntoSublists::usage =
"PartitionIntoSublists[{a,b,c,d,e,f},{1,3,2}] === {{a}, {b, c, d}, {e, g}}\nPartition a list \
into sublists of different lengths.";

AllLessEqual::usage =
"AllLessEqual[{1, 1}, {2, 2}] === True, AllLessEqual[{1, 3}, {2, 2}] === False";

MakeFullFormString::usage =
"Holds an expression and converts it to its FullForm with the context of all symbols expanded";

TraceView::usage =
"Nested graphical browser for Trace result.";

TraceViewFlat::usage =
"Flat graphical browser for Trace result.";

LetL::usage =
"Like With, but later constants can refer to earlier ones.";

JoinTo::usage =
"JoinTo[a_Symbol, b_List] is equivalent to a = a~Join~b"

UpdateRuleList::usage =
"UpdateRuleList[a : {_Rule...}, b : {_Rule...}] updates a with rules in b, prefering those in b over a";

RuleMap::usage = "Equivalent to Normal@AssociationMap"
RuleMapIndexed::usage = "Equivalent to Normal@AssociationMap, but f is always passed the index as well."

PositionFunction::usage =
"PositionFunction[list] returns a function f that returns the position (as an Integer) in the list of a given element,
i.e. list[[PositionFunction[list][x]]] == x if x is in the list.

Missing is returned when the given element is not in the list."

Positions::usage = "Positions[list, elementsInList]
Returns a list of integers representing the positions of the elements in the list, in the same order,
such that
list[[Positions[list, list[[somePositionsInList_List]] ]]] = somePositionsInList"

RulesToFunction::usage =
"RulesToFunction[rules] returns a function f such that f[x] === x /. rules if any rule matches x
(but possibly faster)";

RulesToVerbatimFunction::usage =
"RulesToVerbatimFunction[rules] returns a function f such that f[x] === x /. rules
but does not support any patterns in rules (it's as if you specified any patterns with Verbatim[]).

Note: If speed is key, Association@rules~Lookup~x is faster still,
and a (nested) list with positional lookups even faster (descending into
an expression is pointer-following/array indexing).";

RulesToVerbatimFunction2::usage = "like RulesToVerbatimFunction";

ExternalOutline::usage =
"Compute external morphological gradient ('outer-outline') as a difference between dilated and original image.";

InternalOutline::usage =
"Compute internal morphological gradient ('inner-outline') as a difference between original image and eroded image.";

MatrixInterleave::usage =
"Combines 2 or more matrices into a 'multi-channel' image, similar to
ImageData@Image[matrices, Interleaving->True].";

ArrayInterleave::usage =
    "Combines a list of arrays of the same dimensions into an array of lists.
    Inserts an extra level of lists if only one array is given.";


ArrayDeinterleave::usage =
    "Turns an array of lists into a list of Arrays";

HasAlphaChannel::usage = "Whether 'Transparency' imageMeasurement is true"

NearestFilter::usage =
"NearestFilter[img] Replaces every color in img with the nearest with positive alpha \
(last channel value).

There must be some points with positive last channel value, \
otherwise Nearest::near1 is generated.";

DistanceTransformFull::usage =
"Given a 0-1 image, computes the \
distance of each pixel to the border, such that pixels on the \
boundary get assigned distance 0, inside pixels get negative values \
and outside pixels positive ones."

(* TODO check how heads are handled with the current implementation,
compare with FreeQ (I think that one really considers heads as well,
while MemberQ does not)*)
ContainsQ::usage =
"Not@*FreeQ, or MemberQ[expr, pat, {0,Infinity}, Heads -> True]"

Contains::usage =
"Not@*FreeQ, or MemberQ[expr, pat, {0,Infinity}, Heads -> True]"

ListNormalVectorPlot::usage = "Visualizes a
2d grid of 2d (appending 0 to make them 3d) or 3d vectors
by normalizing them, then rescaling the components to lie around (0<-0.5->1),
then creating an RGB image out of that.

This is a standard encoding of 'normal maps' used in many videogames.

This is applicable whenever ListVectorPlot is."

CrossProductMatrix::usage =
"CrossProductMatrix[w:{x,y,z}].v === Cross[w,v] produces the 'cross-product matrix' for the vector w.

This is also the linear combination w.L where L is the ordered Basis of the Lie-Algebra so(3)
as a 3-dimensional vector space.

This identification is also known as the hat-map.

w can be symbolic in which case the Dot-Product is left unevaluated."

SkewSymmetricMatrix3::usage = "Same as CrossProductMatrix"

Derivative1::usage =
"Derivative[1]@f";

Derivative1D::usage =
"Like Derivative1, but might be more robust with certain functions.";

Derivative1Limit::usage =
"Like Derivative1, but might be more robust with certain functions.";

CallWithUnevaluatedSymbol::usage =
"CallWithUnevaluatedSymbol[f,symbolName_String] causes the \
generation (and evaluation) of the expression f[Unevaluated@symbol], \
where symbol is the Symbol decoded from symbolName using ToExpression.

Notes: This solves the problem of obtaining a vanilla Symbol for a \
named symbol with OwnValues." ;

MemoryBlock::usage =
"MemoryBlock[data, ...] Denotes any list of expressions like List but
has HoldAllComplete to avoid evaluation of any elements. This should
be almost as fast as a packed array, but can be used for arbitrary expressions:

l = Table[x, 10^7];
m = MemoryBlock @@ l;
Do[l[[i]] = y, {i, 1000}] // AbsoluteTiming
Do[m[[i]] = y, {i, 1000}] // AbsoluteTiming

* l takes seconds (the algorithm is quadratic, since l is reevaluated every time
-- why if it is only on the LHS? Do[l[[i]] = l[[i]], {i, 1000}] // AbsoluteTiming
takes much longer still, this is quadratic?),
* m milliseconds.

However, it is slower than a PackedArray for numerical data, try l = Table[0, 10^7]

Use MemoryBlock @@ {l, x} for efficient mixed symbolic-numeric datasets.

An Association is slower than this (especially to construct), but of course supports more complicated
 and sparser keys than dense Position indices.

a = Association@Table[i -> 0, {i, 10^7}];
Do[a[[i]] = 1, {i, 1000}] // AbsoluteTiming

A SparseArray or Dataset are not suitable for large individual indexing.
"


BlockVV::usage =
"BlockVV[vars, body] Like Block, but the variable assignments are specified as \
head[head[var, val], ...] for any heads. Also, var can be any pattern that can appear on the left of an assignment,
not just a symbol.";

FullSymbolName::usage =
"FullSymbolName[x] === Context[Unevaluated@x]<>SymbolName[Unevaluated@x]";

FullName::usage =
"Same as FullSymbolName";


FullNames::usage =
"Same as Names, but keeps the context";

HasOwnValue::usage = "Checks whether the given expression will be replace with anything but itself."

HasDownValues;
ForEach;
FalseQ;

CatchAll::usage =
"Similar to Catch[body, _, f] combined with plain Catch[],
but calls f[value] or f[value, tag] depending on whether there was
an exception with a tag or not. f is not called if there was
no exception.";

PositionsOnLevel::usage = "PositionsOnLevel[e, levelspec]
Returns all positions that point to expressions on levels indicated by
levelspec. Extract[e, PositionsOnLevel[e, levelspec]] == Level[e, levelspec]"

(* TODO support Heads option *)
PositionsToExpressionsOnLevel::usage = "PositionsToExpressionsOnLevel[e, levelspec]
Returns a list of rules Thread@Rule[PositionsOnLevel[e, levelspec], Extract[e, PositionsOnLevel[e, levelspec]]]"


(* TODO implement RulesToExpression, taking position indications and parts
and constructs an expression consisten with this (using List as the head when unknown)
-> generalization of SpraseArray *)

FlatIndexToPosition::usage = "FlatIndexToPosition[i_, base_List] Converts between MixedBase numbers and their digits, but adds an offset 1 to create indices.
For a matrix m, m~Extract~FlatIndexToPosition[i, Dimensions@m] == Flatten@m ~Part~ i"

PositionToFlatIndex::usage = "PositionToFlatIndex[position_List, base_List] Converts between MixedBase numbers and their digits, but adds an offset 1 to create indices.
For a matrix m, m~Extract~pos == Flatten@m ~Part~ PositionToFlatIndex[pos, Dimensions@m]"

ImageNormDifference::usage = "numerical value for estimating similarity of images"
AnyImagePattern::usage = "Image or Image3D"

IsArrayAtLevel::usage = "[array, level] whether array is an array when we look only up to the
indicated level, further in it can be ragged"

ArrayAtLevelQ::usage = IsArrayAtLevel::usage;

GeneralizedTranspose::usage =
"GeneralizedTranspose[a, reordering_, level_]

Returns b with a~Extract~{i,j,k,...} becoming b~Extract~{i,j,k,...}[[reordering]]

Can transpose more arrays than Transpose can?
At least you don't have to specify all indices for reordering
"

UnprotectClearAll::usage = "Like ClearAll, but works with Protected symbols"

AsynchronousEvaluate::usage = "AsynchronousEvaluate[expr_] Uses ParallelSubmit to evaluate expr in another kernel (not
the same as evaluating in a subsession), returning an object that is replaced with the output once it is available.

Use SetSharedVariable[s] on results that you need."

SparseArrayQ::usage = "SparseArrayQ[x] Whether x is syntactically a SparseArray"

AllValues::usage = "AllValues[symbol] which can be given
as a Symbol or String. Returns OwnValues, DownValues, UpValues, SubValues"

OnCoordinateBoundsQ::usage = "True if the integer coordinate point is on the boundary of the integer grid
bounded by the CoordinateBounds style {min,max} array"

LengthQ::usage = "LengthQ[v, len] Length@v === len, operator form: LengthQ[len].

Note that f?g[h] is (f?g)[h], so use ?(LengthQ[...]) in PatternTests involving this"
GivesQ::usage = "GivesQ[v, op, exp] op@v === exp, operator form: GivesQ[op, exp]"
MatchesQ::usage = "MatchesQ[v, op, exp] op@v ~MatchQ ~ exp, operator form: MatchesQ[op, exp]"


FlatCoordinateBoundsArray::usage = "like CoordinateBoundsArray, but gives a list of positions"

EnsurePairs::usage = "EnsurePairs[list, val] Turns a list of pairs \
and non-pairs into a list of pairs, using val for the RHS of any new \
pair."

EchoBeforeAfter::usage = "Like echo, but prints the unevaluated expression before going on"

HasDuplicateQ::usage = "negation of DuplicateFreeQ"

AtomsMatching::usage = "AtomsMatching[e, pat] returns all atoms (level {-1}) matching the given pattern"
SymbolAtoms::usage = "AtomsMatching[e, _Symbol]"

SeriesExtractDerivatives::usage = "for every f and n,

SeriesExtractDerivatives[Series[f@x, {x, y,(*order*)n}]]
===
Table[Derivative[k][f][y], {k, 0, n}]
"

FirstStringPosition::usage = "[string, stringpattern] returns the \
beginning of the first occurrence of a string matching stringpattern \
in string."

TokenizeNestedBracePairs::usage = "TokenizeNestedBracePairs[text] splits text into a nested list of strings,
of the form {\"text with no braces\", {\"[\", {...}, \"]\"}, \"text with no braces\", {\"(\", {...}, \")\"}, ... etc}.
Handles [] () and {}, c.f. TokenizeNestedBracePairsBraces."

$TokenizeNestedBracePairsBraces::usage = "Defines opening and closing braces for TokenizeNestedBracePairs"

DeleteLastWhileItMatches::usage = "DeleteLastWhileItMatches[list, \
pat] remove all last elements from list that match pat"

ReplaceAt::usage = "Like Replace, but applies rules to parts specified by position"
ReplaceLast::usage = "ReplaceAt with position = Last in the expression at level 1."
ReplaceSequence::usage = "Similar to SequenceCases, but leaves the old expression intact,
using the RHS of the rule"

DeleteSequenceCases::usage = "DeleteSequenceCases[e, c] Delete the results from SequenceCases
applied to e, c from e."

KeysValues::usage = "Returns a list containing Keys and Values applied to x"

StringFirstBracePairContents::usage = "StringFirstBracePairContents[\"whatever (contents(y)) (more braces) x\"] returns contents(y)"

BraceAndStringAwareCommaSplit
BraceAwareCommaSplit::usage = "splits a String at commas,
but is aware of braces into which it will not descend.

A routine similar to this is used in C preprocessing of macros."

CommaSplit::usage = "StringSplit with a comma. Leaves non-strings unchanged."
CommaRiffle::usage = "StringRiffle with a comma followed by a space"

SortMost;
MostLast
DropSpace

StringSplitAt

BraceNestingDepth

CharacterMap::usage = "applies a function to each character in s"

IdentityRule

MSBuild

TaskKill

Omittable::usage = "Like Optional, but the element can be missing entirely without having to resort to a default, x | PatternSequence[]"


ParseCStringSource::usage = "Parses s as if it where a bunch of C \
code containing a String literal. Juxtaposed strings are \
concatenated, whitespace outside of the quotes is ignored."

StringFirst

SplitAtFirst

IsideQuotationIndicator::usage = "Returns a list of 0 and 1 for each character in the string,
1 iff the corresponding character is preceded by an odd number of  \" marks"

AllHeads::usage = "[e, h] True iff all heads at level 1 in e are h"

DeleteIf::usage = "Like DeleteCases, but always applies a test. Circumvents the _?f[x] syntax catch."

DeleteMembersOf::usage = "Like Complement, but does not sort"

ToInnerCoordinateBounds::usage = "Given CoordinateBounds, subtracts 1 from the max and adds one to the min,
effectively giving the coordinate bounds of the inner set"

SymmetricMinMax::usage = "Gives a tuple of 2 elements, -a, a where a = Max@Abs@MinMax@data. Useful to scale color gradients with a middle, say RedGreenSplit
for data that is positive/negative"
PlusMinusList::usage = "Gives the ordered tuple {-Abs@x, Abs@x}"

DataAdjust::usage = "Like ImageAdjust but works for any data where Min/Max can be determined"
DataAdjustSymmetric::usage = "Like DataAdjust, but uses SymmetricMinMax to remap 0 to 0.5 and scales the data according to the +- range"

LinearListInterpolation::usage = "LinearListInterpolation[data] Constructs a function in ArrayDepth@data
arguments that linearly (bilinearly, trilinearly etc.) interpolates the data."


ShowDistanceField::usage = "ShowDistanceField[data] visualizes a 2d \
distance field defined by bilinear interpolation of distance values \
defined at grid points"

ShowDistanceField3D::usage = "ShowDistanceField3D[data] visualizes a \
3d distance field defined by bilinear interpolation of distance \
values defined at grid points"

ShowDistanceField3DSlice::usage = "Uses ShowDistanceField to interactively show only one slice at a time (in any direction) through a 3d volume"

(* TODO consider plotting multiple contours at once, make current contour (outlines) thick, c.f. vsfs2d *)

(* ********************* --- Implementation --- ********************* *)

Begin@"`Private`";

(* ********************* ShowDistanceField3D ********************* *)

Options[ShowDistanceField3D] = {Method -> ListPlot,
  PerformanceGoal -> "Speed"};
$ShowDistanceField3DAxesLabel = {"dim1", "dim2",
  "dim3"}(*{"x","y","z"}*)
$ShowDistanceFieldInside = ColorData["RedGreenSplit"]@0;
$ShowDistanceFieldOutside = ColorData["RedGreenSplit"]@1;

ShowDistanceField3D[data_ /; ArrayQ[data, 3, NumericQ],
  Image3D, _] := {smm = SymmetricMinMax@data}~With~Graphics3D[{
  Point@Table[1, 3],
  Translate[
    Raster3D[data~Transpose~{3, 2, 1}
      , ColorFunction -> (ColorData[{"RedGreenSplit", smm}][#]~Append~
        0.5 &)
    ], Table[0.5, 3]]
}
  , Axes -> True
  , AxesLabel -> $ShowDistanceField3DAxesLabel
  , AxesOrigin -> Table[0.5, 3]
  , Ticks -> (Range[#] & /@ Dimensions@data)
  , PlotRange -> ({Table[0.5, 3], 0.5 + Dimensions[data]} //
      Transpose)
]

ShowDistanceField3D[data_ /; ArrayQ[data, 3, NumericQ],
  OptionsPattern[]] :=
    ShowDistanceField3D[data, OptionValue@Method,
      OptionValue@PerformanceGoal];

ShowDistanceField3D[data_ /; ArrayQ[data, 3, NumericQ], ListPlot,
  "Speed"] := {
  smm = SymmetricMinMax@data, cf = ColorData[{"RedGreenSplit", smm}]
  , tdata = data~Transpose~{3, 2, 1}
  , dr = {{1, 1, 1}, Dimensions@data} // Transpose
  , dp = ListDensityPlot3D[tdata
    , ColorFunction -> cf
    , ColorFunctionScaling -> False
    , DataRange -> dr
    , BoxRatios -> Automatic
  ]
  , ticks = (Range[#] & /@ Dimensions@data)
  , lpg =
      If[Max@Dimensions@data < 20,
        "Quality"(*seems faster (less polygons) for small data?
     swapped labels?*), "Speed"]
  , g3d = Graphics3D[Point@Table[1, 3]
    , Axes -> True
    , AxesLabel -> $ShowDistanceField3DAxesLabel
    , AxesOrigin -> Table[1, 3]
    , Ticks -> ticks
  ]
}~LetL~Manipulate[
  Show[
    g3d
    , dp
    , ListContourPlot3D[tdata
    , Contours -> {contour}
    , DataRange -> dr
    , BoxRatios -> Automatic
    , PerformanceGoal -> lpg
    , Mesh -> None
    , ContourStyle -> {FaceForm[$ShowDistanceFieldOutside,
      $ShowDistanceFieldInside]}
  ]
  ],
  {{contour, Max[Min@data, 0]}, Min@data, Max@data},
  TrackedSymbols :> {contour}
]

ShowDistanceField3D[data_ /; ArrayQ[data, 3, NumericQ], ListPlot,
  "Quality"] := {
  f = LinearListInterpolation@data
  , tdata = data~Transpose~{3, 2, 1}
  , smm = SymmetricMinMax@data, cf = ColorData[{"RedGreenSplit", smm}]
  , dr = {{1, 1, 1}, Dimensions@data} // Transpose
  , dp = DensityPlot3D[
    f[x, y, z]
    , {x, 1, Dimensions[data][[1]]}, {y, 1,
      Dimensions[data][[2]]}, {z, 1, Dimensions[data][[3]]}
    , ColorFunction -> cf
    , ColorFunctionScaling -> False
    , PlotRange -> ({{1, 1, 1}, Dimensions[data]} // Transpose)
    , BoxRatios -> Automatic
  (*,OpacityFunctionScaling\[Rule]False,
     OpacityFunction\[Rule](0.1-0.07Sign[#-contour]&)*)
  ]
}~LetL~Manipulate[
  Show[
    Graphics3D[Point@Table[1, 3]
      , Axes -> True
      , AxesLabel -> $ShowDistanceField3DAxesLabel
      , AxesOrigin -> Table[1, 3]
      , Ticks -> (Range[#] & /@ Dimensions@data)
    ]
    , dp
    , ListContourPlot3D[tdata
    , Contours -> {contour}
    , DataRange -> dr
    , PerformanceGoal ->
        If[Max@Dimensions@data < 20, "Speed", "Quality"]
    , BoxRatios -> Automatic
    , Mesh -> None
    , ContourStyle -> {FaceForm[$ShowDistanceFieldOutside,
      $ShowDistanceFieldInside]}
  (*seems nicer for small data?*)
  ]
  ],
  {{contour, Max[Min@data, 0]}, Min@data, Max@data},
  TrackedSymbols :> {contour}
]

(*WARNING: not much better than Quality but much slower, use only for \
Print*)
ShowDistanceField3D[data_ /; ArrayQ[data, 3, NumericQ], ListPlot,
  "HighQuality"] :=
    {tdata = data~Transpose~{3, 2, 1}
      , f = LinearListInterpolation@data
      , smm = SymmetricMinMax@data,
      cf = ColorData[{"RedGreenSplit", smm}]
      , dr = {{1, 1, 1}, Dimensions@data} // Transpose
      , dp = DensityPlot3D[
      f[x, y, z]
      , {x, 1., 1. Dimensions[data][[1]]}, {y, 1.,
        1. Dimensions[data][[2]]}, {z, 1., 1. Dimensions[data][[3]]}
      , ColorFunction -> cf
      , ColorFunctionScaling -> False
      , PlotRange -> ({{1, 1, 1}, Dimensions[data]} // Transpose)
      , BoxRatios -> Automatic
    (*,OpacityFunctionScaling\[Rule]False,
      OpacityFunction\[Rule](0.1-0.07Sign[#-contour]&)*)
    ]
    }~LetL~Manipulate[
      Show[
        Graphics3D[Point@Table[1, 3]
          , Axes -> True
          , AxesLabel -> $ShowDistanceField3DAxesLabel
          , AxesOrigin -> Table[1, 3]
          , Ticks -> (Range[#] & /@ Dimensions@data)
        ]
        , dp
        , ContourPlot3D[(*WARNING this can take forever and hangs up the \
frontend while being transmitted (wstp overhead?!)*)
        f[x, y, z]
        , {x, 1., 1. Dimensions[data][[1]]}, {y, 1.,
          1. Dimensions[data][[2]]}, {z, 1., 1. Dimensions[data][[3]]}
        , Contours -> {contour}
        , PerformanceGoal -> "Quality"
        , BoxRatios -> Automatic
        , Mesh -> None
        , ContourStyle -> {FaceForm[$ShowDistanceFieldInside,
          $ShowDistanceFieldOutside]}(*inside-
      outside are flipped with respect to ListContourPlot3D: bug? *)
      (*seems nicer for small data?*)
      ]
      ],
      {{contour, Max[Min@data, 0]}, Min@data, Max@data},
      TrackedSymbols :> {contour}
    ];

Options[ShowDistanceField3DSlice] = {Method -> ListPlot,
  PerformanceGoal -> "Speed"};

(* TODO improve performance by caching slices*)
ShowDistanceField3DSlice[data_ /; ArrayQ[data, 3, NumericQ],
  o : OptionsPattern[]] := Manipulate[
  Manipulate[
    ShowDistanceField[
      Part[data, Sequence @@ Insert[{All, All}, slice, dim]]
      , o
    ]
    , {{slice, 1}, 1, Dimensions[data][[dim]], 1},
    TrackedSymbols :> {slice}
  ],
  {{dim, 1}, Range@3}, TrackedSymbols :> {dim}
]


(* ********************* ShowDistanceField ********************* *)

ClearAll@ShowDistanceField
Options[ShowDistanceField] = {PerformanceGoal -> "Speed",
  Method -> ListPlot};
$ShowDistanceFieldAxesLabel = {"dim1", "dim2"}(*{"x","y"}*)

ShowDistanceField[data_ /; MatrixQ[data, NumericQ], ListPlot,
  "Speed"] := {smm = SymmetricMinMax@data,
  cf = ColorData[{"RedGreenSplit", smm}], tdata = Transpose@data}~
    LetL~Manipulate[
  Show[
    ListDensityPlot[tdata
      , ColorFunction -> cf
      , ColorFunctionScaling -> False
      , DataRange -> All
      , AspectRatio -> Automatic
      , FrameTicks -> (Range[#] & /@ Dimensions@data)
      , FrameLabel -> $ShowDistanceFieldAxesLabel
    ]

    , ListContourPlot[tdata
    , Contours -> {contour}
    , ContourShading -> None
    , DataRange -> All
  ]
  ],
  {{contour, Max[Min@data, 0]}, Min@data, Max@data},
  TrackedSymbols :> {contour}
]
ShowDistanceField[data_ /; MatrixQ[data, NumericQ], ListPlot,
  "Quality"] := {
  smm = SymmetricMinMax@data, cf = ColorData[{"RedGreenSplit", smm}],
  f = LinearListInterpolation@data
}~LetL~Manipulate[
  Show[
    DensityPlot[
      f[x, y]
      , {x, 1, Dimensions[data][[1]]}, {y, 1, Dimensions[data][[2]]}
      , ColorFunction -> cf
      , ColorFunctionScaling -> False
      , PlotRange -> Full
      , AspectRatio -> Automatic
      , FrameTicks -> (Range[#] & /@ Dimensions@data)
      , FrameLabel -> $ShowDistanceFieldAxesLabel
    ]
    , ContourPlot[f[x, y]
    , {x, 1, Dimensions[data][[1]]}, {y, 1, Dimensions[data][[2]]}
    , Contours -> {contour}
    , ContourShading -> None
  ]
  ],
  {{contour, Max[Min@data, 0]}, Min@data, Max@data},
  TrackedSymbols :> {contour}
]

ShowDistanceField[data_ /; MatrixQ[data, NumericQ], MatrixPlot, _] :=
    MatrixPlot[Transpose@data
      , ImageSize -> Medium
      , ColorFunctionScaling -> False
      , ColorFunction ->
        ColorData[{"RedGreenSplit", SymmetricMinMax@data}]
      , DataReversed -> {True, False}
      , FrameLabel -> Reverse@$ShowDistanceFieldAxesLabel
    ];

ShowDistanceField[data_ /; MatrixQ[data, NumericQ],
  OptionsPattern[]] :=
    ShowDistanceField[data, OptionValue@Method,
      OptionValue@PerformanceGoal];


(* ********************* FullSymbolName ********************* *)

LinearListInterpolation[data_ /; ArrayQ[data, _, NumericQ]] :=
    Interpolation[
      MapIndexed[#2~List~#1 &, data, ArrayDepth@data]~Level~{-3},
      InterpolationOrder -> 1];

DataAdjust[data_] := Rescale[data, MinMax@data];
DataAdjustSymmetric[data_] := {smm=SymmetricMinMax@data, total=2*smm[[2]]}~LetL~(data /total +0.5);

PlusMinusList@x_ := {-Abs@x, Abs@x}
SymmetricMinMax[data_] := PlusMinusList@Max@Abs@MinMax@data

ToInnerCoordinateBounds[cb : {{_Integer, _Integer}..}] := # + {1, -1} & /@ cb;

DeleteIf[x_, t_] := DeleteCases[x, _?t]

AllHeads[e_, h_] := AllTrue[e, Head@# === h&];

(* TODO work with non-chars *)
SplitAtFirst[s_String, c_String] := StringSplitAt[s, FirstStringPosition[s, c]]

StringFirst = StringTake[#,1]&

(*TODO support escaped \"*)
ParseCStringSource[s_String] := StringReplace[s,
  {
    StartOfString ~~ Shortest[Except["\""] ...] ~~ "\"" -> "",
    "\"" ~~ Shortest[Except["\""] ...] ~~ EndOfString -> "",
    "\"" ~~ Shortest[Except["\""] ...] ~~ "\"" -> ""
  }
]

Omittable[x_] := x | PatternSequence[]

TaskKill[exe_String] := RunProcess[{
  "taskkill.exe",  "/f","/im",exe(*note: /im must immediately precede the image name*)
}, "StandardOutput"]

MSBuild[sln_String] := RunProcess[{
  "C:\\Program Files (x86)\\MSBuild\\12.0\\Bin\\msbuild.exe",
  FindFile@sln, "/t:Rebuild"
}, "StandardOutput"]

IdentityRule[x_] := x -> x;

(* avoid very large output hangups *)
(*$PrePrint = If[ByteCount[#] > 10^6, Shallow[#, 1], #] &

(* Causes Issues with Graphics3D, Plot3D and simple things, weird errors like 'Skeleton is not a graphics3D primitive*)
*)


BraceNestingDepth[s_String] := Module[{cnt, depth = 0},
  cnt["("] := ++depth;
  cnt[")"] := depth--;
  cnt[_] := depth;
  CharacterMap[cnt, s]
];

IsideQuotationIndicator[s_String] := Module[{cnt, depth = 0},
  cnt["\""] := depth = 1-depth;
  cnt[_] := depth;
  CharacterMap[cnt, s]
];

CharacterMap[f_, s_String] := f /@ Characters@s;

StringSplitAt[s_String, pos_Integer] := {StringTake[s, pos - 1],
  StringDrop[s, pos]}
StringSplitAt[s_String, {}] := s
StringSplitAt[s_String, pos : {a_Integer, b___Integer}] :=
    Flatten@MapAt[StringSplitAt[#, {b} - a] &, StringSplitAt[s, a], 2];

DropSpace = StringReplace[#, " " -> ""] &

SortMost[{a__, r_}] := Sort@{a}~Append~r;
MostLast[{a__, r_}] := {{a}, r};

CommaRiffle = StringRiffle[#, ", "]&;
CommaSplit = StringSplit[#, ","] &;


CommaSplitIfString[s_String] := StringSplit[s, ","];
CommaSplitIfString[s_] :=StringJoin@Flatten[s];

BraceAwareCommaSplit[s_String] := With[
  {positionsOfnonNestedCommas = Flatten@Position[
    BoolEval[BraceNestingDepth@s == 0]*
        CharacterMap[Boole[# == ","] &, s]
    , 1]
  },
  StringSplitAt[s, positionsOfnonNestedCommas]
]

BraceAndStringAwareCommaSplit[s_String] := With[
  {positionsOfnonNestedCommas = Flatten@Position[
    BoolEval[
        BraceNestingDepth@s == 0] *
        CharacterMap[Boole[# == ","] &, s] *
        (1-IsideQuotationIndicator[s])
    , 1]
  },
  StringSplitAt[s, positionsOfnonNestedCommas]
]

StringFirstBracePairContents[s_String] := Module[{cnt, depth = 0, cont = ""},
  add[x_] := cont = cont<>x;
  cnt[x:"("] := (If[depth>0,add@x];depth++);

  cnt[x:")"] := (depth--;If[depth>0,add@x];If[depth==0,Throw[cont]];);

  cnt[x_] := If[depth>0,add@x];

  Catch@CharacterMap[cnt, s]
];

KeysValues[x_] := Through[{Keys,Values}@x]

DeleteMembersOf[l_, of_] := l~DeleteIf~(MemberQ[of, #] &)

DeleteSequenceCases[e_, c_] := With[{del = SequenceCases[e, c]},
  e~DeleteMembersOf~del
]

ReplaceSequence[expr_List, List[a__]~(Rule|RuleDelayed)~b_] :=
    expr~Replace~({l___, a, r___} :> {l, b, r}) (* TODO support multiple rules , support conditioning
    on the position of the sequence *)

DeleteLastWhileItMatches[{}, pat_] := {};
DeleteLastWhileItMatches[all : {r___, l_}, pat_] :=
    If[MatchQ[l, pat], DeleteLastWhileItMatches[{r}, pat], all];

ReplaceAt[expr_, rules_, positions_] :=
    MapAt[# /. rules &, expr, positions];
ReplaceLast[expr_, rules_] := ReplaceAt[expr, rules, {Length@expr}];

FirstStringPosition[s_String, pat_] :=
    Module[{f = StringPosition[s, pat, 1]},
      If[Length@f > 0, First@First@f, Infinity]
    ];
FirstStringPosition[s_String, ""] = Infinity;

$TokenizeNestedBracePairsBraces = {"[" -> "]", "{" -> "}", "(" -> ")"(*,
  "<"\[Rule]">"*)}
(*nest substrings based on parentheses {([*) (* TODO consider something like http://stackoverflow.com/a/5784082/524504, though non procedural potentially slower*)
TokenizeNestedBracePairs[x_String, closeparen_String] :=
    Module[{opString, cpString, op, cp, result = {}, innerResult,
      rest = x},

      While[rest != "",

        op = FirstStringPosition[rest,
          Keys@$TokenizeNestedBracePairsBraces];
        cp = FirstStringPosition[rest, closeparen];

        Assert[op > 0 && cp > 0];

        Which[
        (*has opening parenthesis*)
          op < cp

          ,(*find next block of [] *)
          result~AppendTo~StringTake[rest, op - 1];
          opString = StringTake[rest, {op}];
          cpString = opString /. $TokenizeNestedBracePairsBraces;
          rest = StringTake[rest, {op + 1, -1}];

          {innerResult, rest} = TokenizeNestedBracePairs[rest, cpString];
          rest = StringDrop[rest, 1];

          result~AppendTo~{opString, innerResult, cpString};

          , cp < Infinity
          ,(*found searched closing parenthesis and no further opening one \
earlier*)
          result~AppendTo~StringTake[rest, cp - 1];
          rest = StringTake[rest, {cp, -1}];
          Return@{result, rest}

          , True
          ,(*done*)
          result~AppendTo~rest; rest = "";
        ]
      ];
      {result, ""}
    ];
(* TODO might want to get rid of empty strings "", { generated here:
TokenizeNestedBracePairs@"f @ g[h[[i[[j[2], k[[1, m[[1, n[2]]]]]]]]]] \
// z"
*)

(* TODO proper error message on unbalanced parentheses*)
TokenizeNestedBracePairs[s_String] :=
    First@TokenizeNestedBracePairs[s, ""]


SeriesExtractDerivatives[
  s : HoldPattern@SeriesData[x_Symbol, ___, k_, den_]] := (Normal@s~
    CoefficientList~(x))/(1/Array[Factorial, k, 0]) (* could also use SeriesCoefficient *)


AtomsMatching[e_, pat_] := Level[e,{-1}]~Cases~pat;
SymbolAtoms[e_] := AtomsMatching[e, _Symbol];
HasDuplicateQ[l_List] := Not@DuplicateFreeQ[l]

EchoBeforeAfter~SetAttributes~HoldAll
EchoBeforeAfter[x_] := (Echo[HoldForm@x,"before"];Echo[x,"after"])

EnsurePairs[{}, _] := {};
EnsurePairs[{has : {x_, v_}, rest___}, val_] :=
    EnsurePairs[{rest}, val]~Prepend~{x, v};
EnsurePairs[{missing_, rest___}, val_] :=
    EnsurePairs[{rest}, val]~Prepend~{missing, val};
EnsurePairs[x_, val_] := {{x, val}};

FlatCoordinateBoundsArray[extents_List] := CoordinateBoundsArray[extents]~Level~{-2}(*~Flatten~(Length@extents-1)*);

PositionsToExpressionsOnLevel[e_, levelspec_] := Thread@Rule[
  PositionsOnLevel[e, levelspec],
  Extract[e, PositionsOnLevel[e, levelspec]]];

LengthQ[v_, len_] := Length@v === len;
LengthQ[len_] := Length@# === len &;

GivesQ[v_, op_, exp_] := op@v === exp;
GivesQ[op_, exp_] := op@# === exp &;

MatchesQ[v_, op_, exp_] := op@v ~MatchQ~ exp;
MatchesQ[op_, exp_] := op@# ~MatchQ~ exp &;

(*for integer coordinates*)
ClearAll@OnCoordinateBoundsQ;
OnCoordinateBoundsQ[p : {__Integer},
  extents : {{_Integer, _Integer} ..}(*CoordinateBounds style*)] /;
    Length@extents == Length@p &&
        AllTrue[extents, Less @@ ## &] :=(*for each coordinate,
  the extents must contain it*)
    Or @@ (#1~ContainsAll~{#2} & @@@ Transpose@{extents, p}); (* TODO use AnyTrue for speedup *)

AllValues[s_Symbol] :=
    Join @@ Through[{OwnValues, DownValues, UpValues, SubValues}[
      Unevaluated@s]]
AllValues[s_String] := CallWithUnevaluatedSymbol[AllValues, s];
AllValues~SetAttributes~HoldAll


SparseArrayQ[x_SparseArray] := True;
SparseArrayQ[_] := False;

(* http://mathematica.stackexchange.com/a/5274/6804*)
qRunTask = CreateScheduledTask[Parallel`Developer`QueueRun[]];
StartScheduledTask[qRunTask];

ClearAll[AsynchronousEvaluate];
SetAttributes[AsynchronousEvaluate, HoldAll];
AsynchronousEvaluate[exp_] := DynamicModule[{eval,display},
  display = eval = ParallelSubmit[exp];
  Dynamic[
    If[MatchQ[eval[[4]],
      Parallel`Developer`finished[_]], display = eval[[4]][[1]]]; display]]

(*first used in ColoredDistanceField from iamge construction*)
ListNormalVectorPlot[x_SparseArray] := ListNormalVectorPlot@Normal@x;


ListNormalVectorPlot[
  arrayOf2dVectors_ /;
      ArrayDepth[arrayOf2dVectors] == 3 &&
          Last@Dimensions@arrayOf2dVectors == 2] :=
    ListNormalVectorPlot@Map[#~Append~0. &, arrayOf2dVectors, {2}];

ListNormalVectorPlot[
  arrayOf3dVectors_ /;
      ArrayDepth[arrayOf3dVectors] == 3 &&
          Last@Dimensions@arrayOf3dVectors == 3] := With[{
  data =
      Map[0.5 + 0.5 If[Norm@# == 0, {0, 0, 1} 1., Normalize@#] &,
        arrayOf3dVectors, {2}]
},
  Image[data, ColorSpace -> "RGB"]
]


UnprotectClearAll~SetAttributes~HoldAll
UnprotectClearAll[x___] := (Unprotect[x];ClearAll[x];)

GeneralizedTranspose[array_, reordering_] :=
    GeneralizedTranspose[array, reordering, Length@reordering];
GeneralizedTranspose[array_, reordering_List?PermutationListQ,
  level_Integer] /; Length@reordering == level && IsArrayAtLevel[array, level] :=
    With[{dim = Dimensions[array][[;; level]],
      inverseReordering = InversePermutation@reordering},
      Array[
        array~Extract~{##}[[inverseReordering]] &(*could also use Permute \
instead of indexed access*)
        , dim[[reordering]]]
    ]

ArrayAtLevelQ = IsArrayAtLevel
IsArrayAtLevel[array_, level_Integer] := ArrayDepth@array >= level
    (* ArrayQ@Map[Null &, array, {level}] (* must be an Array when we look only at the levels *)
*)


(* ********************* FullSymbolName ********************* *)

(* TODO use a function like OptionsPattern[] or an OwnValue?*)
AnyImagePattern[] = _Image|_Image3D;


(* ********************* FullSymbolName ********************* *)

(* if this is 0., images can be considered the same, even though they might not compare ==
 depending on how they where obtained*)
ImageNormDifference[i1 : AnyImagePattern[], i2 : AnyImagePattern[]] := Subtract@@(Norm@*Flatten@*ImageData /@ {i1,i2});

(* ********************* FullSymbolName ********************* *)

HasAlphaChannel[i_] := ImageMeasurements[i, "Transparency"]

(* ********************* FullSymbolName ********************* *)

FirstNonNull~SetAttributes~HoldAll
FirstNonNull[x_] := x;
FirstNonNull[x_, rest__] := {r = x}~With~If[r =!= Null, r,
  FirstNonNull[rest]
  ];

(* ********************* FullSymbolName ********************* *)

AllEqual[list_List, property_] := Equal@@(property /@ list);
AllEqual[property_] := Equal@@(property  /@ #) &;
(* TODO instead of Equal, any operation could be used *)

(* ********************* FullSymbolName ********************* *)

Positions[list_List, elementsInList_List] := With[{pflist = PositionFunction@list},
  pflist /@ elementsInList
  ];

(* ********************* FullSymbolName ********************* *)

FlatIndexToPosition[i_Integer /; i > 0, dimensions : {__Integer}] := IntegerDigits[i-1, MixedRadix@dimensions]+1
PositionToFlatIndex[pos : {__Integer}, dimensions : {__Integer}] := FromDigits[pos-1, MixedRadix@dimensions]+1;
PositionToFlatIndex[pos_Integer, dimensions : {__Integer}] := {pos}~PositionToFlatIndex~dimensions;

(* ********************* FullSymbolName ********************* *)

RuleMap = Normal@*AssociationMap
RuleMapIndexed[f_, list_List] := MapIndexed[list~Extract~#2 -> f@## &, list]

(* ********************* FullSymbolName ********************* *)

PositionFunction[list_List] := Module[{f},
  f[x_] := Missing[PositionFunction, x, "!\[Element]", Short@list]; (* TODO optimize away where it matters *)
  ForEach[{i, Length@list}, f@list[[i]] = i];
  f
]

(* ********************* FullSymbolName ********************* *)

(* TODO support held expressions. Support Heads option? *)
PositionsOnLevel[e_, levelspec_] := Position[e, _, levelspec, Heads->False];

(* ********************* FullSymbolName ********************* *)

HasOwnValue~SetAttributes~HoldAll
HasOwnValue[x_] := Not[x === Unevaluated@x];

(* ********************* FullSymbolName ********************* *)

HoldFirst~SetAttributes~HoldFirst

(* ********************* FullSymbolName ********************* *)

ImportObjVC[file_String]:=Module[{tmp},
tmp=Import[file,"Table"]/.{{"v",rest__}:>TakeDrop[{rest},3],{"f",rest__}:>Polygon[{rest}]};
GraphicsComplex[Cases[tmp,m_?MatrixQ:>m[[1]]],Cases[tmp,_Polygon],VertexColors->Cases[tmp,m_?MatrixQ:>m[[-1]]]]
];

(* ********************* FullSymbolName ********************* *)

FalseQ = Not@*TrueQ;

(* ********************* FullSymbolName ********************* *)

(* like Do, but with the iterator first *)
ForEach~SetAttributes~HoldAll
ForEach[iter_, body_] := Do[body, iter];

(* ********************* FullSymbolName ********************* *)

MemoryBlock~SetAttributes~HoldAllComplete;

(* ********************* FullSymbolName ********************* *)

HasDownValues[symbolName_String] :=
    Length@CallWithUnevaluatedSymbol[DownValues, symbolName] > 0;

(* ********************* FullSymbolName ********************* *)

FullNames[namesPattern_String] := Block[{$Context = "temp`", $ContextPath={}},
  Names@namesPattern];

FullNames[] := Block[{$Context = "temp`", $ContextPath={}},
  Names[]];


(* ********************* FullSymbolName ********************* *)

CallWithUnevaluatedSymbol[f_, symbolName_String] :=
    ToHeldExpression@symbolName /.
        Hold[s_Symbol] :> f@Unevaluated@s;



(* ********************* FullSymbolName ********************* *)

FullSymbolName[s_Symbol] := Context@s <> SymbolName@Unevaluated@s;
FullSymbolName~SetAttributes~HoldAll;
FullName = FullSymbolName;


(* ********************* CatchAll ********************* *)

CatchAll~SetAttributes~HoldAll
CatchAll[body_, f_] := Module[{caught, hadException = True, v},
(*catch 'caught' that we generate*)
  Catch[
  (*catch stuff from body*)
    Throw[
      Hold@
          Evaluate@Catch[(*catch untagged stuff, rethrow with 'caught'*)
            Catch[v = body; hadException = False; v, _,
              Throw[Hold[##], caught] &](*catch tagged stuff,
        rethrow with 'caught'*)
          ], caught]

    , caught
    , If[hadException, f @@ #1, ReleaseHold@#1] &
  ]
];

CatchAll[body_] := With[{caught = Unique[caught]},
(*catch 'caught' that we generate*)
  Catch[
  (*catch stuff from body*)
    Throw[
      Catch[(*catch untagged stuff, rethrow with 'caught'*)
            Catch[body, _,
              Throw[#1, caught] &](*catch tagged stuff,
        rethrow with 'caught'*)
          ], caught]

    , caught
    , #1 &
  ]
];



(* ********************* BlockVV ********************* *)

BlockVV~SetAttributes~HoldRest;
BlockVV::error = "`` = `` failed";

(* TODO this implementation supports only very few variables *)
BlockVV[vv : _[], body_] := body;
BlockVV[vv : h_[_[a_, b_], rest : (_[_, _] ...)], body_] :=
    Catch@Module[{olda = a, hasOld = HasOwnValue@a, v, finally},
      Check[a = b, Message[BlockVV::error, HoldForm[a], HoldForm@b];Throw@$Failed];
      finally := If[hasOld
        , Check[a = olda, Message[BlockVV::error, HoldForm[a], HoldForm@olda];Throw@$Failed]
        , Check[a =.,     Message[BlockVV::error, HoldForm[a], "."];Throw@$Failed]];

      v = CatchAll[
        BlockVV[h[rest], body]
        ,(finally; Throw[##])& (* rethrow - does not return in this case! *)
      ];

      finally; v
    ];

(*
Note that LetL provides something similar for With

TODO implement a variant of Module that supports dependent assignments
*)

(* ********************* Derivative1 ********************* *)

(*The following are different variants to compute the derivative of a function in one argument:*)
Derivative1[f_] := Derivative[1]@f;
Derivative1D[f_] := With[{d = D[f@#, #]}, d &];
Derivative1Limit[f_] := Limit[(f[# + h] - f[#])/h, h -> 0] &;


(* ********************* SkewSymmetricMatrix3 ********************* *)

SkewSymmetricMatrix3[w : {_,_,_}] := {{0, -w[[3]], w[[2]]}, {w[[3]], 0, -w[[1]]},
  {-w[[2]], w[[1]], 0}};

SkewSymmetricMatrix3[w_] := w.SkewSymmetricMatrix3 /@ IdentityMatrix@3;

CrossProductMatrix = SkewSymmetricMatrix3;

(* ********************* ContainsQ ********************* *)

ContainsQ = MemberQ;
Contains = MemberQ;

(* ********************* ColumnVector ********************* *)

ColumnVector[v_?VectorQ] := ColumnVector@{v};
ColumnVector[v_ /; MatrixQ[v] && Dimensions[v]~MatchQ~{_,1}] := v;
ColumnVector[v_ /; MatrixQ[v] && Dimensions[v]~MatchQ~{1,_}] := Transpose@v;
ColumnVector[v_] := Assert[False, {"Cannot convert to a column vector: ", v}];

EnsureList[x_List] := x;
EnsureList[x_] := List@x;


(* ********************* DistanceTransformFull ********************* *)

DistanceTransformFull[img_Image] := Module[{inside, outside},
  inside =
      ImageData@DistanceTransform@img /.
          x_Real /; x > 0. :> x - 1.;(*determine distance of white to black*)

  outside = ImageData@DistanceTransform@ColorNegate@img;
  Image[outside - inside]
]

(* ********************* NearestFilter ********************* *)

NearestFilter[img_Image] := Module[{id, nf, nearestimg},
  id = ImageData@img;
  nf = Nearest[Position[id, {___, _?Positive}]];
  nearestimg = Array[Extract[id,
    First[nf[List@##], 1]
  ] &, Most@Dimensions@id];
  Image[nearestimg, ColorSpace -> ImageColorSpace@img]
];

(* ********************* MatrixInterleave ********************* *)
(* Take a list of matrices and produce a matrix of lists *)
MatrixInterleave[matrices : {_?MatrixQ ..}] :=
    Transpose[matrices, {3, 1, 2}]

(* ********************* ArrayInterleave ********************* *)

ClearAll@ArrayInterleave
(*ArrayInterleave[{a_}, level_Integer] := Map[List, a, {level}];*)

(* Take a list of arrays and produce an array of lists *)
ArrayInterleave[(arrays : {__}), level_Integer] /;IsArrayAtLevel[arrays, level]:=
    (*
    AllTrue[arrays, IsArrayAtLevel[#, level-1]&] &&

    AllEqual[arrays, Dimensions[#][[;;level-1]] &] *)

  Transpose[arrays, Range[level-1] ~Prepend~ level]


ArrayInterleave[a_] := ArrayInterleave[a, ArrayDepth@a]


ArrayDeinterleave[array_, level_Integer] /;IsArrayAtLevel[array, level] := Transpose[array, Range[2,level] ~Append~ 1]

ArrayDeinterleave[a_] := ArrayDeinterleave[a, ArrayDepth@a]

(* ********************* RulesToFunction and RulesToVerbatimFunction ********************* *)

(* :Implementation notes: c.f. LookupTimings *)

RulesToFunction[rules : {___Rule}] := With[{x = Unique[]},
  (x[#1] = #2) & @@@ rules; x
];

RulesToVerbatimFunction[rules : {___Rule}] := With[{x = Unique[], association = Association@rules},
  x[k_] := association~Lookup~k; x
];

RulesToVerbatimFunction2[rules : {___Rule}] := With[{association = Association@rules},
  association~Lookup~# &
];

(* ********************* RulesToFunction and RulesToVerbatimFunction ********************* *)

(* :Implementation notes: c.f. Application (2) of Dilation[] *)

ExternalOutline[i_Image] := ImageSubtract[Dilation[i, 1], i];

InternalOutline[i_Image] := ImageSubtract[i, Erosion[i, 1]];

(* ********************* UpdateRuleList ********************* *)

UpdateRuleList[a : {_Rule...}, b : {_Rule...}] := Module[{aa = Association@a},
  aa~AssociateTo~Association@b;
  Normal@aa
  ];

(* ********************* JoinTo ********************* *)

JoinTo~SetAttributes~HoldFirst;
JoinTo[a_Symbol, b_List] := a = a~Join~b;

(* ********************* LetL === IterateWith ********************* *)

SetAttributes[LetL, HoldAll];
SyntaxInformation[LetL] = {
  "ArgumentsPattern" -> {_, _},
  "LocalVariables" -> {"Solve", {1, Infinity}}
};
LetL /: (assign : SetDelayed | RuleDelayed)[
  lhs_,rhs : HoldPattern[LetL[{__}, _]]
] :=
    Block[{With},
      Attributes[With] = {HoldAll};
      assign[lhs, Evaluate[rhs]]
    ];
LetL[{}, expr_] := expr;
LetL[{head_}, expr_] := With[{head}, expr];
LetL[{head_, tail__}, expr_] :=
    Block[{With}, Attributes[With] = {HoldAll};
    With[{head}, Evaluate[LetL[{tail}, expr]]]];

(* ********************* MakeFullFormString ********************* *)

MakeFullFormString~SetAttributes~HoldAllComplete;
MakeFullFormString[expr_] := Block[{$Context = "temp`", $ContextPath={}},
  ToString@HoldForm@FullForm@expr];

(* ********************* TraceView ********************* *)

SetAttributes[TraceView, {HoldAllComplete}];

TraceView[expr_] :=
    Module[{steps = {}, stack = {}, pre, post, show, dynamic},
      pre[e_] := (stack = {steps, stack}; steps = {})
      ; post[e_, r_] :=
          ( steps = First@stack ~Join~ {show[e, HoldForm[r], steps]}
          ; stack = stack[[2]]
          )
      ; SetAttributes[post, HoldAllComplete]
      ; show[e_, r_, steps_] :=
          Grid[
            steps /. {
              {} -> {{"Expr  ", Row[{e, " ", Style["inert", {Italic, Small}]}]}}
              , _ -> { {"Expr  ", e}
                , {"Steps", steps /.
                    { {} -> Style["no definitions apply", Italic]
                      , _ :> OpenerView[{Length@steps, dynamic@Column[steps]}]}
                }
                , {"Result", r}
              }
            }
            , Alignment -> Left
            , Frame -> All
            , Background -> {{LightCyan}, None}
          ]
      ; TraceScan[pre, expr, ___, post]
      ; Deploy @ Pane[steps[[1]] /. dynamic -> Dynamic, ImageSize -> 10000]
    ]

SetAttributes[TraceViewFlat, {HoldAllComplete}];

TraceViewFlat[expr_] :=
    Module[{steps = {}, stack = {}, pre, post},
      pre[e_] := (stack = {steps, stack}; steps = {})
      ; post[e_, r_] :=
          ( steps = First@stack ~Join~ {{e, steps, HoldForm[r]}}
          ; stack = stack[[2]]
          )
      ; SetAttributes[post, HoldAllComplete]
      ; TraceScan[pre, expr, ___, post]
      ; DynamicModule[{focus, show, substep, enter, exit}
        , focus = steps
        ; substep[{e_, {}, _}, _] := {Null, e, Style["inert", {Italic, Small}]}
        ; substep[{e_, _, r_}, p_] :=
            { Button[Style["show", Small], enter[p]]
              , e
              , Style[Row[{"-> ", r}], Small]
            }
        ; enter[{p_}] := PrependTo[focus, focus[[1, 2, p]]]
        ; exit[] := focus = Drop[focus, 1]
        ; show[{e_, s_, r_}] :=
            Column[
              { Grid[
                { {"Expression", Column@Reverse@focus[[All, 1]]}
                  , { Column[
                  { "Steps"
                    , focus /.
                      { {_} :> Sequence[]
                        , _ :> Button["Back", exit[], ImageSize -> Automatic]
                      }
                  }
                ]
                  , Grid[MapIndexed[substep, s], Alignment -> Left]
                }
                  , {"Result", Column@focus[[All, 3]]}
                }
                , Alignment -> Left, Frame -> All, Background -> {{LightCyan}}
              ]
              }
            ]
        ; Dynamic @ show @ focus[[1]]
      ]
    ]

(* ********************* PartitionIntoSublists ********************* *)

PartitionIntoSublists[l_List, counts : {_Integer ..}] :=
    FoldPairList[TakeDrop, l, counts]; (* consider Internal`PartitionRagged , c.f. http://mathematica.stackexchange.com/questions/7511/partitioning-with-varying-partition-size/123516#123516 *)

PartitionIntoSublists[v_Symbol, counts : {_Integer ..}] :=
    Module[{a, current = 1}, Table[
      a = Array[v[[#]] &, count, current];
      current += count;
      a, {count, counts}]
    ];

(* ********************* AllLessEqual ********************* *)

AllLessEqual[a_List, b_List] := And @@ Thread@LessEqual[a,b];

(* ********************* ArrayReshape extension ********************* *)

ArrayReshape; (* cache it *)
Unprotect@ArrayReshape;

ArrayReshape[v_Symbol, dims : {w_Integer}] :=
    (*Array[v[[#]] &, h]; or *) Table[v[[x]], {x, w}];
ArrayReshape[v_Symbol, dims : {h_Integer, w_Integer}] :=
    Table[v[[(y - 1)*w + x]], {y, h}, {x, w}];
ArrayReshape[v_Symbol, dims : {d_Integer, h_Integer, w_Integer}] :=
    Table[v[[(z - 1)*w*d + (y-1)*w + x]], {z, d}, {y, h}, {x, w}];

Protect@ArrayReshape;


(* ********************* RotationMatrixAxisAngleVector ********************* *)

RotationMatrixAxisAngleVector[w : {_,_,_}] := RotationMatrix[Norm@w, Normalize@w];
RotationMatrixAxisAngleVector[Table[0,3]] := IdentityMatrix@3;
RotationMatrixAxisAngleVector[Table[0|0.,3]] := 1. IdentityMatrix@3;

End[];

EndPackage[];
