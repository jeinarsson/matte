(*
MIT License: https://opensource.org/licenses/MIT

Copyright 2017 Jonas Einarsson

Permission is hereby granted, free of charge, to any person obtaining a copy of this software and associated documentation files (the "Software"), to deal in the Software without restriction, including without limitation the rights to use, copy, modify, merge, publish, distribute, sublicense, and/or sell copies of the Software, and to permit persons to whom the Software is furnished to do so, subject to the following conditions:

The above copyright notice and this permission notice shall be included in all copies or substantial portions of the Software.

THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY, FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM, OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE SOFTWARE.
*)

BeginPackage["Matte`"]

ClearAll["Matte`*"];

(* Generating indices *)
gHighestIndex = 20; (*idx[1] to idx[20] are reserved for manual input*)
getNewIndex[] := Module[{},
   gHighestIndex = gHighestIndex + 1;
   idx[gHighestIndex]
   ];
getNewIndex[n_Integer] := Table[getNewIndex[], {k, 1, n}]
getLastIndex[] := idx[gHighestIndex];

(* Symmetries and dummy index renaming *)
Clear[renumber, renumberUnique,
   symmetryRules,
  symmetricTensorPattern, antisymmetricTensorPattern, tracelessTensorPattern];

symmetricTensorPattern = delta | e | T[_,_,_] | Q | \[Theta];
antisymmetricTensorPattern = o | op | eps;
tracelessTensorPattern = a | e | Q| \[Theta]; 

symmetryRules = {

  (* Shuffle indices into sorted order, and in case of antisymmetric tensors multiplies with (-1) *)
  s_[before___, ii : idx[i_Integer], jj : idx[j_Integer], after___] :> s[before, jj, ii, after] /; j < i && MatchQ[s, symmetricTensorPattern],
  s_[before___, i_Integer, j_Integer, after___] :> s[before, j, i, after] /; j < i && MatchQ[s, symmetricTensorPattern],
  s_[before___, ii : idx[i_Integer], jj : idx[j_Integer], after___] :> -s[before, jj, ii, after] /; j < i && MatchQ[s, antisymmetricTensorPattern],
  s_[before___, i_Integer, j_Integer, after___] :> -s[before, j, i, after] /; j < i && MatchQ[s, antisymmetricTensorPattern],

  (* tracelessness *)
  s_[___, i_idx, ___, i_idx, ___] :> 0 /; MatchQ[s, tracelessTensorPattern] || MatchQ[s, antisymmetricTensorPattern],

  (* two-index contraction of symmetric and antisymmetric tensors give 0 *)
  s1_[___,i_idx,___, j_idx,___] s2_[idcs__] :> 0 /; (MatchQ[s1, antisymmetricTensorPattern] && MatchQ[s2, symmetricTensorPattern]) && ContainsAll[List[idcs], {i, j}],
  s1_[___,i_idx,___, j_idx,___] s2_[idcs__] :> 0 /; (MatchQ[s1, symmetricTensorPattern] && MatchQ[s2, antisymmetricTensorPattern]) && ContainsAll[List[idcs], {i, j}],
  s1_[___,i_idx,___, j_idx,___] v_[i_] v_[j_] :> 0 /; MatchQ[s1, antisymmetricTensorPattern]
};


(* 
Renumber
*)

(* Don't renumber if there are no indices *)
renumber[ex_] := ex /; FreeQ[ex, idx]

(* Map over terms in sums *)
renumber[ex_Plus] := Map[renumber, ex]

(* If free of sums, do actual renumbering *)
renumber[expr_] := Module[{ex, exidxs,
    allidx, idxWithCounts, freeIndices, dummyIndices, highestFreeIndex,
    newDummyIndices, dummyPositions, dummyHeads, dummyOrdering, dummyRules},
   
   ex = expr //. symmetryRules;
   exidxs = ex /. s_[idxsequence : _idx ..]^2 :> idxs[idxsequence, idxsequence];
   allidx = Cases[exidxs, idx[i_Integer], Infinity, Heads -> True];
   idxWithCounts = Tally[allidx];
   freeIndices = First /@ Select[idxWithCounts, #[[2]] == 1 &];
   dummyIndices = First /@ Select[idxWithCounts, #[[2]] == 2 &];
   
   highestFreeIndex = If[Length[freeIndices] > 0,
     Max[freeIndices /. idx[n_] :> n],
     0];
   
   newDummyIndices = Table[idx[highestFreeIndex + k], {k, 1, Length[dummyIndices]}];
   dummyPositions = (Position[ex, #]) & /@ dummyIndices;
   dummyPositions = Map[If[Length[#] > 1, First /@ #, First[#]] &, dummyPositions];
   dummyHeads = Map[Head@Extract[ex, #] &, dummyPositions, {2}];
   dummyOrdering = Ordering[dummyHeads];
   dummyRules = Table[
     dummyIndices[[dummyOrdering[[k]]]] -> newDummyIndices[[k]]
     , {k, 1, Length[dummyIndices]}];
   
   (ex /. dummyRules) //. symmetryRules
   ] /; FreeQ[ex, _Plus?(Not[FreeQ[#, idx]] &)]

(* Don't renumber if there are no indices *)
renumberUnique[ex_] := ex /; FreeQ[ex, idx]

(* Map over terms in sums *)
renumberUnique[ex_Plus] := Map[renumberUnique, ex]

(* If free of sums, do actual renumbering *)
renumberUnique[expr_] := Module[{ex, exidxs,
    allidx, idxWithCounts, freeIndices, dummyIndices,
    newDummyIndices, dummyPositions, dummyHeads, dummyOrdering, dummyRules},
   
   ex = expr //. symmetryRules;
   exidxs = ex /. s_[idxsequence : _idx ..]^2 :> idxs[idxsequence, idxsequence];
   allidx = Cases[exidxs, idx[i_Integer], Infinity, Heads -> True];
   idxWithCounts = Tally[allidx];
   freeIndices = First /@ Select[idxWithCounts, #[[2]] == 1 &];
   dummyIndices = First /@ Select[idxWithCounts, #[[2]] == 2 &];
   
   newDummyIndices = Table[getNewIndex[], {k, 1, Length[dummyIndices]}];
   dummyPositions = (Position[ex, #]) & /@ dummyIndices;
   dummyPositions = Map[If[Length[#] > 1, First /@ #, First[#]] &, dummyPositions];
   dummyHeads = Map[Head@Extract[ex, #] &, dummyPositions, {2}];
   dummyOrdering = Ordering[dummyHeads];
   dummyRules = Table[
     dummyIndices[[dummyOrdering[[k]]]] -> newDummyIndices[[k]]
     , {k, 1, Length[dummyIndices]}];
   
   (ex /. dummyRules) //. symmetryRules
   ] /; FreeQ[ex, _Plus?(Not[FreeQ[#, idx]] &)]


(****************
CONTRACTION AND MANIPULATION 
*****************)

(* Differentiation *)
Clear[partialr];

(* Nest multiple derivatives, eg partialr[ex, {i,j}]=partialr[partialr[ex,i],j]*)
partialr[ex_, idxs_List] := Fold[partialr, ex, idxs] 

(* Linearity *)
partialr[ex_Plus, i_] := Map[partialr[#, i] &, ex] 

(* Product rule *)
partialr[ex_Times, i_] := Take[ex, 1] partialr[Drop[ex, 1], i] + Drop[ex, 1] partialr[Take[ex, 1], i]

(* derivative of constants wrt r = 0 *)
partialr[ex_, i_] := 0 /; FreeQ[ex, invr | r | rhat] 

(* 1/r^m *)
partialr[invr[m_], i_] := -m r[i] invr[m + 2] 

(* r_i *)
partialr[r[i_], j_] := delta[i, j] 

(* shorthand for laplacian *)
laplacer[ex_] := Module[{j}, j = getNewIndex[];
  partialr[ex, {j, j}]
  ]



(* Tensor rules *)

Clear[tensorRules, transpose, trace]
Clear[choose2pairs]

(* Helper for T-tensors: generate (n choose 2) permutations of pairs from list *)
choose2pairs[list_] := Flatten[Table[
   Module[{},
    {Delete[list, {{i1}, {i2}}], {list[[i1]], list[[i2]]}}
    ]
   , {i1, 1, Length[list] - 1}, {i2, i1 + 1, Length[list]}
   ], 1]

(* Traces *)
transpose[s : symmetricTensorPattern] := s
transpose[s : antisymmetricTensorPattern] := -s
trace[before___, -x_, after___] := -trace[before, x, after]
trace[tracelessTensorPattern] := 0;
trace[antisymmetricTensorPattern] := 0;
trace[delta ..] := 3
sqr[delta]:=3

(* Replacement rules *)
tensorRules = {
   (* treat invr's *)
   r[i_idx]^2 :> invr[-2],
   invr[m1_] invr[m2_] :> invr[m1 + m2],
   invr[m_]^n_ :> invr[m n],
   invr[0] :> 1,
   
   (* treat invk's *)
   k[i_idx]^2 :> invk[-2],
   invk[m1_] invk[m2_] :> invk[m1 + m2],
   invk[m_]^n_ :> invk[m n],
   invk[0] :> 1,
   
   (* contract deltas with other tensors *)
   delta[i_, j_idx] s_ :> (s /. j -> i) /; Not[FreeQ[s, j]],
   delta[j_idx, i_] s_ :> (s /. j -> i) /; Not[FreeQ[s, j]],
   delta[i_idx,j_Integer]^2 :> 1,
   delta[i_Integer, j_Integer] :> KroneckerDelta[i,j],

   (* levi-civita contractions*)
   eps[i_idx,k_,l_]eps[i_, p_, q_] :> delta[k,p]delta[l,q]-delta[k,q]delta[l,p],
   eps[i_idx,k_,l_]eps[q_, i_, p_] :> delta[k,p]delta[l,q]-delta[k,q]delta[l,p],
   eps[i_idx,k_,l_]eps[p_, q_, i_] :> delta[k,p]delta[l,q]-delta[k,q]delta[l,p],
   eps[l_, i_idx, k_]eps[i_, p_, q_] :> delta[k,p]delta[l,q]-delta[k,q]delta[l,p],
   eps[l_, i_idx, k_]eps[q_, i_, p_] :> delta[k,p]delta[l,q]-delta[k,q]delta[l,p],
   eps[l_, i_idx, k_]eps[p_, q_, i_] :> delta[k,p]delta[l,q]-delta[k,q]delta[l,p],
   eps[k_, l_, i_idx]eps[i_, p_, q_] :> delta[k,p]delta[l,q]-delta[k,q]delta[l,p],
   eps[k_, l_, i_idx]eps[q_, i_, p_] :> delta[k,p]delta[l,q]-delta[k,q]delta[l,p],
   eps[k_, l_, i_idx]eps[p_, q_, i_] :> delta[k,p]delta[l,q]-delta[k,q]delta[l,p],
   
   (* traces & squares *)
   s_[_idx]^2 :> sqr[s],
   s_[i_idx, i_idx] :> trace[s],
   s_[i_idx, j_idx]^2 :> trace[s, transpose[s]],
   s_[i_idx, j_idx] s_[j_idx, i_idx] :> trace[s, s],
   
   
   (* treat T-tensors *)
   T[0, 0, kr_][] :> 1,
   T[n_, l_, kr_][idc__] :> 0 /; n < l,
   
   T[n_, l_, kr_][before___, i : idx[_Integer], i_, after___] :> 
    T[n - 2, l, kr][before, after],
   
   T[n_Integer, l_Integer, kr_][idc__] :> Module[{pairs},
      pairs = choose2pairs[List[idc]];
      2/(n (n + 1) - l (l + 1)) Sum[
        T[n - 2, l, kr][Sequence @@ p[[1]]] delta[Sequence @@ p[[2]]], {p, 
         pairs}]
      ] /; n > l
   };


(* Contract function *)
Clear[contract, contractNoRenumber];

(* Join tensor and symmetry rules *)
tensorAndSymmetryRules = Join[tensorRules, symmetryRules];

(* Map over lists of expressions *)
contractNoRenumber[ex_List] := Map[contractNoRenumber, ex];

(* distribute over terms in sum *)
contractNoRenumber[ex_Plus] := Map[contractNoRenumber, ex];

(* If no sums in expression, apply rules until convergence *)
contractNoRenumber[ex_] :=  
  FixedPoint[Expand[# /. tensorAndSymmetryRules] &, ex] /; FreeQ[ex, _Plus?(Not[FreeQ[#, idx]] &)];

(* If there are sums, Expand and try again *)
contractNoRenumber[ex_] := contractNoRenumber[Expand[ex]];

(* Main function: contract first, then renumber result *)
contract[ex_] := renumber[contractNoRenumber[ex]];

(*************
 PRETTY PRINTING 
 *************)

(* Split expressions into scalars & tensorial components *)
Clear[splitScalarsTensors];
tensorPattern=idx|sqr|trace;
splitScalarsTensors[ex_Times] := Module[{temp},
    (* gather scalar and tensorial part *)
    
    temp = GroupBy[List @@ ex, (FreeQ[#, tensorPattern] &)];
    
    (* multiply factors back together *)
    temp = Map[Times @@ # &, temp];
    
    {Lookup[temp, True, 1], Lookup[temp, False, 1]}
    ] /; FreeQ[ex, _Plus?(Not[FreeQ[#, idx]] &)];
splitScalarsTensors[ex_] := 
  If[FreeQ[ex, tensorPattern], {ex, 1}, {1, ex}] /; 
   FreeQ[ex, _Plus?(Not[FreeQ[#, idx]] &)];
splitScalarsTensors[ex_] := 
 Print["splitScalarsTensors: Error: Sum of tensorial expressions."]


Clear[prettyprint, prettyreplace]
(* this helper function applies the replacement rules *)
prettyreplace[ex_] := Module[{temp, prettyindices},
  prettyindices = {i, j, k, l, p, q, r, s};
  temp = ex /. {
     invr[m_] :> 1/r^m,
     delta -> \[Delta],
     eps -> \[Epsilon]
     };
  
  (* subscript index notation *)
  temp = temp /. s_[i : _idx ..] :> Subscript[s, i];
  
  (* numbered indices to letters *)
  temp = temp /. Table[idx[c] -> prettyindices[[c]], {c, 1, Length[prettyindices]}]
  ]

(* groups terms by tensorial expression and then calls prettyreplace *)
prettyprint[ex_] := Module[{temp},
  
  (* If not sum, nothing to collect, so just call prettyreplace *)
  If[Not[Head[ex] === Plus], Return[prettyreplace[ex]]];
  
  (* make list of terms in sum *)
  temp = List @@ ex;
  
  (* split each term into scalar and tensorial factors *)
  temp = splitScalarsTensors /@ temp;
  
  (* Group terms with equal tensorial expressions *)
  temp = GroupBy[temp, Last];
  
  (* Combine and simplify scalar prefactors *)
  temp = Map[Simplify@First@Total[#] &, temp];
  
  (* Convert association back to normal expression *)
  temp = prettyreplace @ Total @ KeyValueMap[Times, temp]
  ]


(* Extract all tensorial forms of expression *)
Clear[getTensorialForms];
getTensorialForms[ex_] := Module[{temp},
  
  (* If not sum, nothing to collect, so just call splitScalarsTensors *)
  If[Not[Head[ex] === Plus], Return[{splitScalarsTensors[ex][[1]]}]];
  
  (* make list of terms in sum *)
  temp = List @@ ex;
  
  (* split each term into scalar and tensorial factors *)
  temp = splitScalarsTensors /@ temp;
  
  (* Extract unique tensorial factors *)
  temp = Map[Last, temp]//DeleteDuplicates;

  (* Prettyprint result *)
  Map[prettyprint@renumber[#]&, temp]

  ]  


(* Extract all scalar prefactors of expression *)
Clear[getScalarPrefactors];
getScalarPrefactors[ex_] := Module[{temp},
  
  (* If not sum, nothing to collect, so just call splitScalarsTensors *)
  If[Not[Head[ex] === Plus], Return[{splitScalarsTensors[ex][[1]]}]];
  
  (* make list of terms in sum *)
  temp = List @@ ex;
  
  (* split each term into scalar and tensorial factors *)
  temp = splitScalarsTensors /@ temp;
  
  (* Group terms with equal tensorial expressions *)
  temp = GroupBy[temp, Last];
  
  (* Combine and simplify scalar prefactors *)
  temp = Map[Simplify@First@Total[#] &, temp];
  
  Values[temp]
  ]

  (* Extract all scalar prefactors of expression *)
Clear[printStructured];
printStructured[ex_] := Module[{temp,scalars,tensors},
  
  (* If not sum, nothing to collect, so just call splitScalarsTensors *)
  If[Not[Head[ex] === Plus], Return[{splitScalarsTensors[ex][[1]]}]];
  
  (* make list of terms in sum *)
  temp = List @@ ex;
  
  (* split each term into scalar and tensorial factors *)
  temp = splitScalarsTensors /@ temp;
  
  (* Group terms with equal tensorial expressions *)
  temp = GroupBy[temp, Last];
  
  (* Combine and simplify scalar prefactors *)
  scalars = Map[Simplify@First@Total[#] &, temp];
  
  tensors = Map[prettyprint@renumber@Last@First[#] &, temp];
  
  {Values[scalars], Values[tensors]} //Transpose// MatrixForm[#,TableAlignments->{Right}]&
  ]
  

(************
INTEGRATION
*************)

(* Generate unique pairings for isotropic tensors*)
Clear[uniquePairings];
uniquePairings[{a_, b_}] := {{{a, b}}} (* base case n=2 *)
uniquePairings[list_] := Flatten[Table[
   Table[Prepend[p, {list[[1]], list[[k]]}], {p, 
     uniquePairings[Delete[list, {{1}, {k}}]]}]
   , {k, 2, Length[list]}], 1]


Clear[angularIntegral, unitsphereIntegral, fluidVolumeIntegral];

 (* linearity *)
angularIntegral[ex_Plus] := Map[angularIntegral, ex]

(* trivial integrals *)
angularIntegral[1] := 4 Pi invr[-2] 
angularIntegral[r[_]] := 0

(* take everything except r[_idx] outside the angular integral *)
angularIntegral[extra_ (withr_: 1)] := 
 contractNoRenumber[extra angularIntegral[withr]] /; FreeQ[extra, r]

(* general integral of r[i_]r[j_]... *)
angularIntegral[HoldPattern[rs : Times[r[_] ..]]] := Module[{rindices, numr, integral},
  
  (* list indices of r *)
  rindices = Cases[List @@ rs, r[i : _idx] :> i];
  numr = Length[rindices];
  
  (* odd integral = 0 *)
  If[OddQ[numr], Return[0]];
  
  (* even integral: form all permutations of deltas with rindices and the prefactors *)
  integral = ((4 \[Pi])/(1 + numr))/((2^(numr/2) Gamma[1/2 + numr/2])/
      Sqrt[\[Pi]]) Sum[
     Product[delta[pp[[1]], pp[[2]]], {pp, p}],
     {p, uniquePairings[rindices]}];
  
  invr[-numr - 2] integral
  ]

(* Common application: integral over unit sphere *)
unitsphereIntegral[ex_] := angularIntegral[ex] /. invr[_] :> 1 // contractNoRenumber

(* Volume integral for r>1 *)
fluidVolumeIntegral[ex_] := Module[{x, a},
  a = contractNoRenumber[angularIntegral[ex]] /. invr[m_] :> 1/x^m;
  If[Head[a] === Plus,
    Map[Integrate[# , {x, 1, Infinity}] &, a]
    ,
    Integrate[a , {x, 1, Infinity}]
    ] // contract
  ]


(***************
FOURIER TRANSFORM
****************)

(* Convert between polyadics and T-tensors *)

Clear[toTtensors];

(* trivial case *)
toTtensors[ex_] := ex /; FreeQ[ex, r | k]

(* operate on one term at a time *)
toTtensors[ex_Plus] := Map[toTtensors, ex] // contract

(* extract all factors that are independent of r or k *)
toTtensors[extra_ (withrk_: 1)] := 
 contractNoRenumber[extra toTtensors[withrk]] /; FreeQ[extra, r[_] | k[_]]

(* match polyads in r and convert to sum of T-tensors *)
toTtensors[HoldPattern[(rs : Times[r[_] ..])]] := Module[{n, idc},
  n = Length[rs];
  idc = List @@ rs /. r[i : _idx] :> i;
  invr[-n] Total[Table[T[n, l, r][Sequence @@ idc], {l, n, 0, -2}]] // 
   contractNoRenumber
  ]
toTtensors[r[i : idx_]] := invr[-1] T[1, 1, r][i]

(* match polyads in k and convert to sum of T-tensors *)
toTtensors[HoldPattern[(ks : Times[k[_] ..])]] := Module[{n, idc},
  n = Length[ks];
  idc = List @@ ks /. k[i : _idx] :> i;
  invk[-n] Total[Table[T[n, l, k][Sequence @@ idc], {l, n, 0, -2}]] // 
   contractNoRenumber
  ]
toTtensors[k[i : idx_]] := invk[-1] T[1, 1, k][i]


Clear[toPolyadics];

(* Recursive replacement to lower values of l, and finally the trivial case *)
toPolyadics[ex_] := (
    ex //. {
      T[l_, l_, kr_][idc__] :> 
       Product[T[1, 1, kr][j], {j, List[idc]}] - 
         Sum[contractNoRenumber[T[l, j, kr][idc]], {j, l - 2, 0, -2}] /; 
        l > 1
      }
      ) //. {T[1, 1, r][idx_] :> invr[1] r[idx], T[1, 1, k][idx_] :> invk[1] k[idx]} // contract


(* Fourier transform of T-tensor expressions *)
Clear[fourierT, fourier]

(* fourier and invfourier works on polyadics *)
fourier[ex_] := ex // toTtensors // fourierT // toPolyadics
invfourier[ex_] := ex // toTtensors // invfourierT // toPolyadics

(* linearity *)
fourierT[ex_Plus] := Map[fourierT, ex] 
(* take everything except invr and r (incl T[_,_,r]) outside the transform *)
fourierT[extra_ (withr_: 1)] := contractNoRenumber[extra fourierT[withr]] /; FreeQ[extra, r | invr]

(* linearity *)
invfourierT[ex_Plus] := Map[invfourierT, ex] 
(* and take everything except invk and k (incl T[_,_,k]) outside the transform *)
invfourierT[extra_ (withk_: 1)] := contractNoRenumber[extra invfourierT[withk]] /; FreeQ[extra, k | invk]

(* prefactors *)
Psi[n_, l_] := (-I)^ l 2^(n + 3) Pi^(3/ 2) (Gamma[(l + 3 + n)/2])/(Gamma[(l - n)/2])/(Sqrt[2 Pi]^3)

(* the general case *)
fourierT[T[l_, l_, r][idc__]] := Psi[0, l] invk[3] T[l, l, k][idc] /; 
  Not[l <= 0 && EvenQ[l]] && Not[-l - 3 >= 0 && EvenQ[-l - 3]]

(* two edge cases m=0 and l=0 *)
fourierT[invr[m_]] := Psi[-m, 0] invk[3 - m] /;
  Not[m <= 0 && EvenQ[m]] && Not[m - 3 >= 0 && EvenQ[m - 3]]
fourierT[invr[m_] T[l_, l_, r][idc__]] := Psi[-m, l] invk[3 - m] T[l, l, k][idc] /;
  Not[m + l <= 0 && EvenQ[m + l]] && Not[m - l - 3 >= 0 && EvenQ[m - l - 3]]

(* same for invfourier, where we use that F^-1[f](r)=F[f](-r) (gives factor of (-1)^l) *)
invfourierT[invk[m_]] := Psi[-m, 0] invr[3 - m] /;
  Not[m <= 0 && EvenQ[m]] && Not[m - 3 >= 0 && EvenQ[m - 3]]
invfourierT[invk[m_] T[l_, l_, k][idc__]] := (-1)^l Psi[-m, l] invr[3 - m] T[l, l, r][idc] /; 
  Not[m + l <= 0 && EvenQ[m + l]] && Not[m - l - 3 >= 0 && EvenQ[m - l - 3]]
invfourierT[T[l_, l_, k][idc__]] := (-1)^l Psi[0, l] invr[3] T[l, l, r][idc] /;
  Not[l <= 0 && EvenQ[l]] && Not[-l - 3 >= 0 && EvenQ[-l - 3]]


Print["Matte package loaded. © 2017-2018 Jonas Einarsson (me@jonaseinarsson.se)"]
Print["Released under the MIT License https://opensource.org/licenses/MIT"]

EndPackage[]