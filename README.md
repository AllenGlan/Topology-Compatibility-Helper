# Topology-Compatibility-Helper
A Wolfram Language script that rewrites multi-qubit gates to help IBMQASM-based algorithms run on universal quantum computers (QTMs) of varied topology.

## What does the script do?

The script is meant to be simple: it takes input in the QASM format, parses desired topology and commands from it, rewrites multi-qubit gates (particularly the CNOT gate, since almost all multi-qubit gates are composed of CNOT and single-qubit gates) in accordance with the topology, and streams the results to the output. 

## Why do we need gate rewriting?

At the current stage, multi-qubit QTMs are almost always constructed in a way that qubits and the connections between them constitute less than a complete digraph. Hence algorithms developed on *ideal* $n$-qubit systems may not necessarily be compatible with *real* $n$-qubit hardware, as CNOT gates may call for quantum connections not present on the computer. Gate rewriting offers a detour by

1. channeling a CNOT gate between two indirectly connected qubits through the shortest path between them, and
2. inverting the direction of a CNOT gate by wrapping each qubit with a pair of Hadamard gates to create superposition, in compliance with the direction of the corresponding quantum connection.

Hence the sufficient and necessary condition for the topology of an $n$-qubit QTM to be compatible with an arbitrary $n$-qubit or less-than-$n$-qubit algorithm could be reduced from **being a complete digraph** to **being weakly connected**.