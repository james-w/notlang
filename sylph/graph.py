def copy_graph(graph):
    """Copies a graph, but does not copy the nodes."""
    new_graph = {}
    for a, b in graph.items():
        new_graph[a] = []
        for c in b:
            new_graph[a].append(c)
    return new_graph


def expand_edges(graph):
    """Fully expand edge graph transitively.
    
    Given an edge graph

        node: [node, node, ...]

    this function will expand the list of edges
    transitively until no more expansion can be done.

    In the case of a cycle the node will be added to
    its own list, but will proceed no further.
    """
    graph = copy_graph(graph)
    changed = True
    while changed:
        changed = False
        for a, b in graph.items():
            for c in b:
                if c in graph:
                    for d in graph[c]:
                        if d not in b:
                            b.append(d)
                            changed = True
    return graph


def get_disjoint_sets(graph, nodes):
    """Pull out sets of nodes from a directed cyclic graph.

    Given a list of nodes, and map of edges in the form

       node: [node, node, ...]

    this function will pull out a list of sets of nodes,
    where each set depends only on nodes in its own set
    or earlier, aiming for the largest number of sets.

    For instance with [a, b, c, d] and [a: [b], b: [a], c: [a]],
    then [[a, b], [c], [d]] would satisfy the criteria. [d] comes
    last as it depends on nothing, and nothing depends on it.
    Next comes [c], which depends on a, so the set with a has to
    come before it. Then [a, b] are together, as they have a
    cyclical dependency.

    We use this to look at a dependency graph, and pull out
    a processing order where we can be sure to have processed
    all dependendencies before things that depend on them.

    We make no promises that the function use graph theory
    best practice, is efficient, or is even correct.
    """
    graph = expand_edges(graph)
    chains = []
    to_add = nodes[:]
    def in_list_list(a, l):
        for lb in l:
            if a in lb:
                return True
        return False
    while to_add:
        candidate = to_add.pop()
        add_it = True
        circular = False
        chain = []
        if candidate in graph:
            if candidate in graph[candidate]:
                circular = True
                chain = graph[candidate]
        for a, b in graph.items():
            if a not in chain:
                if candidate in b:
                    if not in_list_list(b, chains):
                        add_it = False
        if add_it:
            if circular:
                chain = graph[candidate]
                chains.insert(0, chain)
                for c in chain:
                    if c in graph:
                        del graph[c]
                    if c in to_add:
                        to_add.remove(c)
            else:
                chains.insert(0, [candidate])
                if candidate in graph:
                    del graph[candidate]
        else:
            to_add.insert(0, candidate)
    return chains
