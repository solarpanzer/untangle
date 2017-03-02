module DependencyGraph

open QuickGraph
open QuickGraph.Graphviz

type Node = string
type Edge = Node * Node
type Graph = {nodes:Set<Node>; edges:Set<Edge>}

module Qg = 
    type QgNode = string
    type QgEdge = SEdge<string>
    type QgGraph = AdjacencyGraph<QgNode, QgEdge>
        
    let transitiveHullForNode (graph:QgGraph) (node:QgNode) =
        let mutable hull = []
        let dfs = Algorithms.Search.DepthFirstSearchAlgorithm(graph)
        dfs.add_DiscoverVertex(fun v -> hull <- v :: hull)
        dfs.SetRootVertex(node)
        dfs.Compute()
        hull |> Set.ofSeq

    let graphToDot graph = 
        let formatVertex (e : FormatVertexEventArgs<Node>) = 
            e.VertexFormatter.Label <- e.Vertex
        let graphvizAlgorithm = GraphvizAlgorithm<QgNode, QgEdge>(graph)
        graphvizAlgorithm.FormatVertex.Add(formatVertex)
        graphvizAlgorithm.Generate()

    let toQgGraph g = 
        let wrappedEdges = g.edges |> Seq.map (fun (a,b) -> QgEdge(a,b))
        wrappedEdges.ToAdjacencyGraph()

let fromNodesAndEdges nodeObjects edgeTuples = 
    { nodes = Set.ofSeq nodeObjects; edges = Set.ofSeq edgeTuples }
    
let filterNodes predicate g = 
    let newNodes = g.nodes |> Set.filter predicate
    let newEdges = g.edges |> Set.filter (fun (a,b) -> (predicate a) && (predicate b))
    fromNodesAndEdges newNodes newEdges

let mapNodesMergeEdges graph nodeMapping =
    let newNodeSet = graph.nodes |> Seq.map nodeMapping |> Set.ofSeq
    let mapEdge (s, t) = (nodeMapping s, nodeMapping t)
    let selfDependency (a,b) = (b = a)
    let newEdgeSet = graph.edges |> Seq.map mapEdge |> Seq.filter (not << selfDependency) |> Set.ofSeq
    fromNodesAndEdges newNodeSet newEdgeSet

let graphToDot graph = graph |> Qg.toQgGraph |> Qg.graphToDot

let graphToCsv graph =
    let boolToSymbol b = if b then "X" else ""
    let lineForSource source = 
        seq {
            yield source
            yield! graph.nodes |> Seq.map (fun target -> Set.contains (source, target) graph.edges |> boolToSymbol)
        } |> String.concat "," 
    seq {
        yield String.concat "," ("" :: (graph.nodes |> List.ofSeq))
        yield! graph.nodes |> Seq.map lineForSource
    } |> String.concat "\n"

let csvToGraph (csvString:string) =
    let lines = csvString.Split('\n')
    let nodeNames = lines.[0] |> fun s -> s.Split(',') |> Seq.skip 1 |> Array.ofSeq
    let edgeLineToEdgePairs (line:string) =
        match line.Split(',') |> List.ofArray with
            | source :: entries ->
                entries 
                |> List.mapi (fun i v -> (i, v))
                |> List.filter (fun (i,v) -> v.Length > 0)
                |> List.map (fun (i,v) -> (source, nodeNames.[i]))
            | _ -> []   
    let edges = 
        let edgeLines = lines.[1..]
        edgeLines |> Seq.collect edgeLineToEdgePairs
    fromNodesAndEdges nodeNames edges

let transitiveHullForNodeSet graph nodes = 
    let qg = Qg.toQgGraph graph
    nodes |> Seq.collect (Qg.transitiveHullForNode qg) |> Set.ofSeq

let transitiveHullForNode graph node = 
    Seq.singleton node |> transitiveHullForNodeSet graph

let invert graph =
    let inverseEdges = [for (a,b) in graph.edges -> (b,a)]
    fromNodesAndEdges graph.nodes inverseEdges