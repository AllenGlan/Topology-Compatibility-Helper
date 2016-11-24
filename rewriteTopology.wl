#!/usr/local/bin/wolframscript

(*
==========================================
TOPOLOGY COMPATIBILITY HELPER
For IBM Universal Quantum Computers
Version: 0.1.2, IBMQASM 1.1
Author: Allen Glan
Date: November 22, 2016
==========================================
*)


rewriteTopology[qasmPath_,outputPath_]:=Block[
	{
		undRewAppFunc,dirRewAppFunc,
	
		undRewExp,dirRewLnr,

		qasmStream,qasmList,qasmCursor,qasmLength,initPos,cxAbsPos,optnState,optnStart,optnEnd,optnPos,topoState,topoList,topoGraph ,topoUndGraph,qasmCmd,cmdLength,cxList,cxRouteList,	cxUndRew,cxDirRew,cxRewPair,cxOutList,outputList,cmdNewLength,outputStream
	},

	(* REWRITE FUNCTIONS *)
	
	undRewAppFunc=undRewExp;
	dirRewAppFunc=dirRewLnr;

	(* Undirected rewriting O(1.5*2^E-2) Exponential *)
	undRewExp[route_]:=Block[{heap,routeGroup},
		routeGroup=Partition[route,2,1];
		heap=routeGroup[[1]];
		Do[
			heap=Sequence[heap,n,heap,n],
			{n,Drop[routeGroup,1]}
		];
		(*heap=Flatten[heap,1];*)
		Return[{heap}];
	];

	(* Directed rewriting O(3E) Linear *)
	dirRewLnr[routeGroup_,topoList_]:=Block[{comp,dirRewSgl,assem},
		comp=With[{n=ToExpression@topoList},Thread[{Keys@n,Values@n}]];
		dirRewSgl[route_]:=If[Position[comp,route]=={},
			Return[Hold@Sequence[
				"h "<>ToString[route[[2]]]<>";",
				"h "<>ToString[route[[1]]]<>";",
				"cx "<>ToString[route[[2]]]<>", "<>ToString[route[[1]]]<>";",
				"h "<>ToString[route[[2]]]<>";",
				"h "<>ToString[route[[1]]]<>";"
			]],
			Return["cx "<>ToString[route[[1]]]<>", "<>ToString[route[[2]]]<>";"]
		];
		assem=Map[dirRewSgl,routeGroup,{2}];
		Return[ReleaseHold@assem];
	];

	(* INITIALIZATION *)

	qasmList={};
	qasmCursor="";
	qasmLength=0;
	initPos={};
	cxAbsPos={};
	(* Define Option States and Substates *)
	optnState=False;
	topoState=False;
	(* Define Option Positions *)
	optnStart={};
	optnEnd={};
	optnPos={};
	(* Topology *)
	topoList={};
	topoGraph=Null;
	(* Pure command chain *)
	qasmCmd={};
	cmdLength=0;
	cxList={};
	cxRouteList={};
	outputList={};
	cmdNewLength={};

	(* READING INPUT *)

	Print[""];

	(* Check file existence *)
	If[FileExistsQ[qasmPath]==False,
		Print["[ERROR] "<>qasmPath<>" not found!"];
		Exit[]
	];

	Print["[INFO] Reading from "<>qasmPath<>"."];

	qasmStream=OpenRead[qasmPath];

	While[qasmCursor!="EndOfFile",
		qasmCursor=ReadLine[qasmStream];
		AppendTo[qasmList,qasmCursor];
		qasmLength+=1;

		(* Find IBMQASM initialization *)
		If[StringCases[ToString@qasmCursor,"//IBMQASM"~~__]!={},
			AppendTo[initPos,{qasmLength}]
		];

		(* Note cx absolute positions *)
		If[StringCases[ToString@qasmCursor,"cx q["~~__~~"], q["~~__~~"]"]!={},
			AppendTo[cxAbsPos,{qasmLength}]
		];

		(* Trigger I/O of Option State *)
		If[StringCases[ToString@qasmCursor,"/*"]!={},
			optnState=True;
			AppendTo[optnStart,qasmLength]
		];
		If[StringCases[ToString@qasmCursor,"*/"]!={},
			optnState=False;
			AppendTo[optnEnd,qasmLength]
		];

		(* Deny all substates when Option State is disabled *)
		If[optnState==False,topoState=False];

		(* Start Topology logging *)
		If[topoState==True,
			AppendTo[topoList,qasmCursor];
		];

		(* Enable Topology State *)
		If[optnState==True,
			If[StringCases[ToString@qasmCursor,"Topology"]!={},
				topoState=True
			];
		];

	];

	Close[qasmStream];
	qasmLength-=1;
	qasmList=Drop[qasmList,-1];

	(* FINDING TOPOLOGY DEFINITIONS *)

	optnPos=Partition[Flatten@Table[Range@ReplaceAll[n,List->Sequence],{n,Thread[{optnStart,optnEnd}]}],1];
	
	topoGraph=Graph[ToExpression@topoList,DirectedEdges->True];
	topoUndGraph=Graph[ToExpression@topoList,DirectedEdges->False];
	(*topoDiagram=Graph[topoGraph,VertexLabels\[Rule]"Name",EdgeLabels\[Rule]"Name",GraphLayout\[Rule]"StarEmbedding",VertexSize\[Rule]Medium];*)
	
	qasmCmd=Delete[qasmList,Join[initPos,optnPos]];
	cmdLength=Length[qasmCmd];
	
	(* Error-handling *)
	If[qasmLength<=1||Length@initPos!=1||WeaklyConnectedGraphQ[topoGraph]!=True,
		Print["[ERROR] QASM file corrupted!"];
		Exit[],
		Print["[INFO] QASM input legit."]
	];

	Print["[INFO] Read "<>ToString@cmdLength<>" rules."];

	(* UNDIRECTED REWRITING *)
	cxList=Table[
		ToExpression@ToString@{StringDrop[n,3]},
		{n,Flatten@StringCases[qasmCmd,"cx q["~~__~~"], q["~~__~~"]"]}
	];
	cxRouteList=Table[FindShortestPath[topoUndGraph,ReplaceAll[n,List->Sequence]],{n,cxList}];
	cxUndRew=Table[undRewAppFunc[n],{n,cxRouteList}];

	(* DIRECTED REWRITING *)
	cxDirRew=dirRewAppFunc[cxUndRew,topoList];
	cxRewPair=Normal@AssociationThread[Flatten@cxAbsPos,cxDirRew];
	cxOutList=Flatten@ReplacePart[qasmList,cxRewPair];

	Print["[INFO] CNOT rewriting completed."];

	(* GENERATING OUTPUT LIST *)
	outputList=cxOutList;
	cmdNewLength=Length@outputList-qasmLength+cmdLength;

	Print["[INFO] Generated "<>ToString@cmdNewLength<>" rules."];
	Print["[INFO] Performance Overhead: "<>ToString@NumberForm[N[100(cmdNewLength-cmdLength)/cmdLength],{5, 2}]<>"%"];
	
	(* WRITING TO FILE *)
	outputStream=OpenWrite[outputPath];
	Do[WriteLine[outputStream, n], {n, outputList}];
	Close[outputStream];
	Print["[INFO] Successfully streamed to "<>outputPath<>"."];
	
];
(* End of Block *)

rewriteTopology[ToString[$ScriptCommandLine[[2]]],ToString[$ScriptCommandLine[[3]]]];

Exit[];