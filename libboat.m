Begin["BoatContext`"];

(* DEBUG THING *)
ClearAll[debug];
SetAttributes[debug, HoldAll];
debug[code_] := 
 Internal`InheritedBlock[{Message}, 
  Module[{inMessage}, Unprotect[Message];
   Message[args___] /; ! MatchQ[First[Hold[args]], _$Off] := 
    Block[{inMessage = True}, 
      Print[{Shallow /@ 
           Replace[#, HoldForm[f_[___]] :> HoldForm[f], 1], 
          Style[Map[Short, Last[#], {2}], Red]} &@
        Drop[Drop[Stack[_], -7], 4]];
      Message[args];
      Throw[$Failed, Message];] /; ! TrueQ[inMessage];
   Protect[Message];];
  Catch[StackComplete[code], Message]]

(* Find all roots function *)
Clear[findAllRoots]
SyntaxInformation[
   findAllRoots] = {"LocalVariables" -> {"Plot", {2, 2}}, 
   "ArgumentsPattern" -> {_, _, OptionsPattern[]}};
SetAttributes[findAllRoots, HoldAll];

Options[findAllRoots] = 
  Join[{"ShowPlot" -> False, PlotRange -> All}, 
   FilterRules[Options[Plot], Except[PlotRange]]];

findAllRoots[fn_, {l_, lmin_, lmax_}, opts : OptionsPattern[]] := 
 Module[{pl, p, x, localFunction, brackets}, 
  localFunction = ReleaseHold[Hold[fn] /. HoldPattern[l] :> x];
  If[lmin != lmax, 
   pl = Plot[localFunction, {x, lmin, lmax}, 
     Evaluate@
      FilterRules[Join[{opts}, Options[findAllRoots]], Options[Plot]]];
   p = Cases[pl, Line[{x__}] :> x, Infinity];
   If[OptionValue["ShowPlot"], 
    Print[Show[pl, PlotLabel -> "Finding roots for this function", 
      ImageSize -> 200, BaseStyle -> {FontSize -> 8}]]], p = {}];
  brackets = 
   Map[First, 
    Select[(*This Split trick pretends that two points on the curve \
are "equal" if the function values have _opposite _ sign.Pairs of \
such sign-changes form the brackets for the subsequent FindRoot*)
     Split[p, Sign[Last[#2]] == -Sign[Last[#1]] &], 
     Length[#1] == 2 &], {2}];
  x /. Apply[FindRoot[localFunction == 0, {x, ##1}] &, 
     brackets, {1}] /. x -> {}]

(* Begin Notebook to Package conversion code *)

convert[notebook_, package_] := 
 Module[{nb, str}, nb = NotebookOpen[notebook(*,Visible\[Rule]False*)];
  SelectionMove[nb, All, Notebook];
  str = First[
    FrontEndExecute[
     FrontEnd`ExportPacket[NotebookSelection[nb], "InputText"]]];
  (*NotebookClose[nb];*)
  Export["text.txt", str];
  Quiet[DeleteFile[package]];
  RenameFile["text.txt", package];]

(* End Notebook to Package conversion code *)

(* Standard Boat Library *)
(*Needs["CUDALink`"];*)
Needs["CUDALink`"];

(* Generate point cloud from BoundaryMeshRegion *)

pointCloud[reg_?RegionQ, numPoints_] :=
  RandomPoint[reg, numPoints];

sortCompiled = Compile[{{pts, _Real, 2}, {normal, _Real, 1}},
   Sort[#.normal & /@ pts]];

ClearAll[sortPoints];
sortPoints[pts_, normal_] := sortPoints[pts, normal] =
   sortCompiled[pts, normal];

(* So many implementations *)
ClearAll[csortPoints];
csortPoints[pts_, normal_] :=
  CUDASort[CUDADot[pts, N[normal]]];

ClearAll[msortPoints];
msortPoints[pts_, normal_] :=
  Sort[#.normal & /@ pts];

compiledsortPoints[pts_, normal_] :=
  sortCompiled[pts, normal];

(* Test the implementations' speeds *)

stressTest[pts_, times_] :=
  
  Module[{normal, cudaTiming, cpuTiming, compiled},
   normal = Normalize[{0, Sin[x], Cos[x]}];
   cudaTiming = 
    Timing[Table[csortPoints[pts, normal], {x, Range[times]}]][[1]];
   cpuTiming = 
    Timing[Table[msortPoints[pts, normal], {x, Range[times]}]][[1]];
   compiled = 
    Timing[Table[
       compiledsortPoints[pts, normal], {x, Range[times]}]][[1]];
   Grid[{
     {"cuda", cudaTiming},
     {"cpu", cpuTiming},
     {"compiled", compiled}
     }]
   ];

canLocationFinder[comHull_, distanceFromCOM_, deltaHeight_] :=
  
  Module[{},
   {comHull + {distanceFromCOM, 0, deltaHeight}, 
    comHull + {-distanceFromCOM, 0, deltaHeight}}
   ];

mastLocationFinder[comHull_, deltaHeight_] :=
  Module[{},
   comHull + {0, 0, deltaHeight}
   ];

ballastLocationFinder[comHull_, deltaHeight_] :=
 
 comHull + {0, 0, deltaHeight}

regionBounds[boat_] :=
  Module[{hull},
   hull = region /. Select[boat, #[type] == "Buoy" &][[1]];
   RegionBounds[hull]
   ];

exportBoat[boat_, stlLocation_, filename_] :=
  
  Module[{boatFiltered, waterLineAssoc, stlAssoc},
   boatFiltered = KeyTake[boat, {name, type, com}];
   waterLineAssoc = <|"name" -> "Water", 
     "waterline" -> calculateWaterLine[boat, {0, 0, 1}]|>;
   stlAssoc = <|"name" -> "STL", "location" -> stlLocation|>;
   
   boatFiltered = Join[boatFiltered, {waterLineAssoc, stlAssoc}];
   
   (* Wow this is ....ed up *)
   
   boatFiltered = <|
       KeyValueMap[# -> 
          If[QuantityQ[#2] || (Length[#2] > 1 && QuantityQ[#2[[1]]]), 
           QuantityMagnitude[#2], #2] &, #]|> & /@ boatFiltered;
   
   Export[
    filename, <|KeyValueMap[ToString[#1] -> #2 &, #]|> & /@ 
     boatFiltered]
   ];

ClearAll[waterLineOld];
(* Returns the index that puts a certain portion of the volume \
underwater *)

waterLineOld[pts_, normal_, meshVolume_, volumeUnderwater_] :=
  
  waterLineOld[pts, normal, meshVolume, volumeUnderwater] = 
   Module[{numPoints, dists},
    numPoints = volumeUnderwater/meshVolume * Length[pts];
    dists = sortPoints[pts, normal];
    (* Possible improvement: 
    linear interpolate between values at two points *)
    
    dists[[Floor[numPoints]]]
    ];

ClearAll[waterLine];
waterLine[boat_, normal_, desiredVolumeUnderwater_] :=
  
  waterLine[boat, normal, desiredVolumeUnderwater] =
   Module[{buoys, buoy, bPoints, fullySubmergedVolume, 
     indexOfWaterLine, pointDistances},
    buoys = Select[boat, #[type] == "Buoy" &];
    (* Only one buoy object per boat is allowed, 
    due to different point masses *)
    
    If[Length[buoys] != 1, 
     Throw["No don't do that, one buoy per boat please"]];
    buoy = buoys[[1]];
    
    (* These operations are for many buoys, 
    but only one buoy is valid *)
    
    fullySubmergedVolume = volume /. buoy;
    bPoints = points /. buoy;
    
    (* Find how many points are underwater *)
    
    indexOfWaterLine = 
     desiredVolumeUnderwater/fullySubmergedVolume*Length[bPoints];
    
    (* Index must be in range of points *)
    
    If[indexOfWaterLine < 1, Throw["Index not >= 1"]];
    If[indexOfWaterLine > Length[bPoints], 
     Throw["Index > number of points"]];
    
    pointDistances = sortPoints[bPoints, normal];
    
    (* Possible improvement: 
    linear interpolate between values at two points *)
    
    pointDistances[[Floor[indexOfWaterLine]]]
    ];

ClearAll[calculateVolumeUnderwater];
calculateVolumeUnderwater[boat_] :=
  calculateVolumeUnderwater[boat] =
   Module[{boatMass, displacedWaterVolume},
    boatMass = 
     Sum[p[mass], {p, Select[boat, #[type] == "PointMass" &]}];
    
    displacedWaterVolume = boatMass/waterDensity
    ];

ClearAll[calculateWaterLine];
calculateWaterLine[boat_, wNormal_] :=
 
 calculateWaterLine[boat, wNormal] =
  waterLine[boat, wNormal, calculateVolumeUnderwater[boat]]

ClearAll[calculateForces];
calculateForces[boat_, wNormal_] :=
  calculateForce[boat, wNormal] =
   Module[{buoys, buoy, masses, weightForces, buoyancyForce},
    buoys = Select[boat, #[type] == "Buoy" &];
    (* Assume prior exception thrown if not 1 element *)
    
    buoy = buoys[[1]];
    
    masses = Select[boat, #[type] == "PointMass" &];
    
    (* Calculate the forces *)
    
    buoyancyForce = 
     calculateVolumeUnderwater[boat]*waterDensity*g*wNormal;
    weightForces = #*(g*-wNormal) & /@ (mass /. masses); (* 
    Tricky replacement bullshit *)
    
    <|
     wfs -> weightForces,
     bf -> buoyancyForce
     |>
    ];

compiledBelowWaterLine = 
  Compile[{{pts, _Real, 2}, {wNormal, _Real, 1}, {waterHeight, _Real}},
   Select[pts, #.wNormal <= waterHeight &]];

(* Find the points of the buoyHull of a boat when translational \
forces are at equib *)
ClearAll[pointsBelowWaterLine];
pointsBelowWaterLine[boat_, wNormal_] :=
  
  pointsBelowWaterLine[boat, wNormal] =
   Module[{buoy, waterHeight},
    buoy = Select[boat, #[type] == "Buoy" &][[1]];
    waterHeight = calculateWaterLine[boat, wNormal];
    
    (* Dot products, get all points that are below the water height *)

        compiledBelowWaterLine[points /. buoy, wNormal, waterHeight]
    ];

(* Find the total COM for the boat *)
ClearAll[calculateCOM];
calculateCOM[boat_] :=
  calculateCOM[boat] =
   Module[{masses, coms, ms},
    masses = Select[boat, #[type] == "PointMass" &];
    coms = com /. masses;
    ms = mass /. masses;
    (Plus @@ (coms*ms))/
     Plus @@ ms
    ];

(* Find the COB of a boat when translational forces are at equib *)

ClearAll[calculateCOB];
calculateCOB[boat_, wNormal_] :=
  calculateCOB[boat, wNormal] =
   Module[{points},
    points = pointsBelowWaterLine[boat, wNormal];
    Quantity[Sum[p, {p, points}]/Length[points], "Centimeters"]
    ];

(* Find the moment vectors that the boat has *)

ClearAll[calculateMoments];
calculateMoments[boat_, wNormal_] :=
  
  calculateMoments[boat, wNormal] =
   Module[{buoys, buoy, masses, forces, buoyancy, weights, buoyR, 
     buoyMoment, weightsR, weightsMoments},
    buoys = Select[boat, #[type] == "Buoy" &];
    (* Assume prior exception thrown if not 1 element *)
    
    buoy = buoys[[1]];
    
    masses = Select[boat, #[type] == "PointMass" &];
    
    (* Calculate the forces *)
    
    forces = calculateForces[boat, wNormal];
    weights = wfs /. forces;
    buoyancy = bf /. forces;
    
    (* Buoyancy moment *)
    buoyR = calculateCOB[boat, wNormal];
    buoyMoment = Cross[buoyR, buoyancy];
    
    (* Calculate weights moments *)
    weightsR = com /. masses;
    weightsMoments = Cross @@@ Transpose[{weightsR, weights}];
    
    (* Return the moments *)
    Prepend[weightsMoments, buoyMoment]
    ];

vectorCircle[up_, front_] :=
  front\[Cross]up*Sin[#] +
    up*Cos[#] &;

avsDefaults = {UpVec -> {0, 0, 1}, FrontVec -> {1, 0, 0}};

(* Calculate the righting arm distance *)
ClearAll[rightingArm];
Options[rightingArm] = avsDefaults;
rightingArm[boat_, wNormal_, opts : OptionsPattern[]] :=
  
  rightingArm[boat, wNormal] =
   Module[{buoys, buoy, masses, cob, com, left, r},
    buoys = Select[boat, #[type] == "Buoy" &];
    (* Assume prior exception thrown if not 1 element *)
    
    buoy = buoys[[1]];
    
    masses = Select[boat, #[type] == "PointMass" &];
    
    cob = calculateCOB[boat, wNormal];
    com = calculateCOM[boat];
    
    left = wNormal\[Cross]Normalize[OptionValue[FrontVec]];
    r = cob - com;
    r.left
    ];

(* Calculate the AVS function, interpolated from a number of points *)

ClearAll[calculateAVS];
Options[calculateAVS] = avsDefaults;
calculateAVS[boat_, samplePoints_, range_, opts : OptionsPattern[]] :=

    calculateAVS[boat, samplePoints, range, opts] =
   Module[{angleVec, avses, interp},
    angleVec = vectorCircle[OptionValue[UpVec], OptionValue[FrontVec]];
    
    avses = ParallelTable[
      rightingArm[boat, angleVec[\[Theta]], opts],
      {\[Theta], range[[1]], 
       range[[2]], (range[[2]] - range[[1]])/samplePoints}
      ];
    
    ListInterpolation[
     avses,
     {range[[1]], range[[2]]}
     ]
    ];

(* Get intersection points in the AVS function *)

getAVSPoints[fAVS_, range_] :=
  
  findAllRoots[
   QuantityMagnitude[fAVS[\[Theta]]], {\[Theta], range[[1]], 
    range[[2]]}];

(* Plot -Pi to Pi for AVS *)
Options[plotCircleAVS] = avsDefaults;
plotCircleAVS[boat_, samplePoints_, opts : OptionsPattern[]] :=
  
  plotAVS[boat, samplePoints, {-Pi, Pi}, opts];

Options[plotAVS] = avsDefaults;
plotAVS[boat_, samplePoints_, range_, opts : OptionsPattern[]] :=
  
  Module[{maxAngle = Pi, avses, interp, avsPoints},
   interp = 
    calculateAVS[boat, samplePoints, range, 
     FilterRules[{opts}, Options[calculateAVS]]];
   avsPoints = getAVSPoints[interp, range];
   
   Plot[
    interp[\[Theta]],
    {\[Theta], range[[1]], range[[2]]},
    PlotLabel -> "AVS",
    Ticks -> {Range[range[[1]], 
       range[[2]], (range[[2]] - range[[1]])/8], Automatic},
    AxesLabel -> Automatic,
    Epilog -> {
      {Point[{#, 0}] & /@ avsPoints}
      }
    ]
   ];

Options[manyBoats] = 
  Join[avsDefaults, {ViewPoint -> {-\[Infinity], 0, 0}}];
manyBoats[boat_, samplePoints_, opts : OptionsPattern[]] :=
  
  Module[{maxAngle = Pi, angleVec},
   angleVec = 
    vectorCircle[OptionValue[UpVec], OptionValue[FrontVec]];
   ParallelTable[
    Show[
     renderBoatWithForces[boat, angleVec[\[Theta]]],
     ViewPoint -> OptionValue[ViewPoint]
     ],
    {\[Theta], -maxAngle, maxAngle, (maxAngle*2)/samplePoints}
    ]
   ];

renderBoat[boat_, wNormal_] :=
  
  Module[{wl, waterPlane, pointMassParts, buoyParts, cob, frontVec},
   (* Calculate waterline to plot plane on *)
   
   wl = calculateWaterLine[boat, wNormal];
   waterPlane = {Blue, Opacity[0.2], HalfSpace[wNormal, wNormal*wl]};
   
   (* Grab all point masses to render *)
   
   pointMassParts = Select[boat, #[type] == "PointMass" &];
   
   (* Render buoy *)
   
   buoyParts = Select[boat, #[type] == "Buoy" &][[1]];
   cob = QuantityMagnitude[calculateCOB[boat, wNormal]];
   
   Graphics3D[{
     waterPlane, render /. pointMassParts,
     (cobRender /. buoyParts)@cob
     }
    ]
   ];

renderBoatWithForces[boat_, wNormal_] :=
  
  Module[{forces, wForces, bForce, momentSum, com, scale, arrow, text,
     wVecs, bVec, mVecs, mText},
   
   forces = UnitConvert[calculateForces[boat, wNormal], "Newtons"];
   
   wForces = wfs /. forces;
   bForce = bf /. forces;
   
   momentSum = UnitConvert[
     Plus @@ calculateMoments[boat, wNormal],
     "NewtonMeters"];
   
   com = QuantityMagnitude[calculateCOM[boat]];
   
   scale = 0.5;
   
   arrow = Line[{com, com + (QuantityMagnitude[#]*scale)}] &;
   text = Text[#, com + (QuantityMagnitude[#]*scale)] &;
   
   wVecs = {Red, arrow /@ wForces};
   bVec = {Blue, arrow@bForce};
   (* Arbitrary numbers to scale *)
   
   mVecs = {Purple, arrow@(momentSum*20)};
   
   mText = {text@momentSum};
   
   Show[renderBoat[boat, wNormal], Graphics3D[{wVecs, bVec, mVecs}]]
   ];

boatSummary[boat_] :=
  
  Module[{up = {0, 0, 1}, range = {-Pi, Pi}, avsPoints, avsPointsDisp,
     avsTrim},
   
   avsPoints = getAVSPoints[
      calculateAVS[boat, 20, range, FrontVec -> {1, 0, 0}],
      range]/Degree;
   
   avsPointsDisp = 
    Grid[{Table[d Quantity[1, "AngularDegrees"], {d, avsPoints}]}];
   
   avsTrim = (\[Theta] /. 
       FindRoot[
        QuantityMagnitude[
         calculateAVS[boat, 20, range, 
           FrontVec -> {0, 1, 0}][\[Theta]]],
        {\[Theta], 0}])/Degree;
   
   Grid[{
     {"Boat", Show[renderBoat[boat, up], ImageSize -> Medium]},
     {"AVS Plot", Show[plotAVS[boat, 20, range], ImageSize -> Medium]},
     {"AVS Zeros", avsPointsDisp},
     {"Trim AVS", 
      Show[plotAVS[boat, 20, range, FrontVec -> {0, 1, 0}], 
       ImageSize -> Medium]},
     {"Trim", avsTrim Quantity[1, "AngularDegrees"]}
     }, Frame -> All]
   ];

convert["libboat.nb", "libboat.m"]

End[];

