// Learn more about F# at http://fsharp.org
// See the 'F# Tutorial' project for more help.
open System
open System.Text.RegularExpressions

let readGraphFromDotFile fileName = 
    let dotParserGraph = IO.File.ReadAllText(fileName) |> DotParser.parse
    let keysOfMap map = map |> Map.toSeq |> Seq.map fst |> Seq.toArray
    let allNodes = dotParserGraph.Nodes |> keysOfMap
    let allEdges = dotParserGraph.Edges |> keysOfMap
    DependencyGraph.fromNodesAndEdges allNodes allEdges
    
let regexPredicate pattern = 
    let regex = Regex(pattern)
    fun s -> regex.IsMatch(s)

module Untangle =
    let nodesMatchingRegexes (graph : DependencyGraph.Graph) (regexes : seq<string>) =
        let filterWithRegex nodes pattern = nodes |> Seq.filter (regexPredicate pattern)
        regexes |> Seq.collect (filterWithRegex graph.nodes)

    let hull (graph : DependencyGraph.Graph) (rootDefiningRegexes : seq<string>) = 
        let rootNodes = nodesMatchingRegexes graph rootDefiningRegexes
        DependencyGraph.transitiveHullForNodeSet graph rootNodes
    
    let inversehull (graph : DependencyGraph.Graph) (rootDefiningRegexes : seq<string>) =
        hull (DependencyGraph.invert graph) rootDefiningRegexes
    
    let edgecheck (graph : DependencyGraph.Graph) (sourceRegex : string) (targetRegex : string) =
        let sourcePredicate = regexPredicate sourceRegex
        let targetPredicate = regexPredicate targetRegex
        let predicate (s,t) = (sourcePredicate s) && (targetPredicate t)
        graph.edges |> Seq.filter predicate
    
    let checkcsv (graph : DependencyGraph.Graph) (csvFileName : string) =
        let csvString = System.IO.File.ReadAllText(csvFileName)
        let csvGraph = DependencyGraph.csvToGraph csvString
        graph.edges |> Seq.filter (fun e -> not <| Set.contains e csvGraph.edges)

    let clusterByDependentNodeCoverage (graph : DependencyGraph.Graph) (rootDefiningRegexes : seq<string>) =
        let rootNodes = nodesMatchingRegexes graph rootDefiningRegexes

        let nodesGroupedByRootNodeDependencies = 
            let hulls = rootNodes |> Seq.map (DependencyGraph.transitiveHullForNode graph >> Set.ofSeq) |> Array.ofSeq
            let hullMembershipsForNode n = hulls |> Array.map (Set.contains n)
            graph.nodes |> Seq.groupBy hullMembershipsForNode |> Seq.map snd
    
        let nodeToGroupLabelMap =
            let mappingsForGroup group = 
                let groupLabel = group |> String.concat "\n"
                let keyValuePair node = (node, groupLabel)
                group |> Seq.map keyValuePair
            nodesGroupedByRootNodeDependencies |> Seq.collect mappingsForGroup |> Map.ofSeq

        let lookup k = defaultArg (Map.tryFind k nodeToGroupLabelMap) "???"

        DependencyGraph.mapNodesMergeEdges graph lookup

module Commands =
    let (|PrefixNoCase|_|) (p:string) (s:string) =
        if s.ToLower().StartsWith(p.ToLower()) then
            Some(s)
        else
            None

    let edgeToArrow (s,t) = s + " -> " + t
        
    let run graph argList =
        match argList with
            | PrefixNoCase "hull" _ :: rest -> 
                printfn "%s" (Untangle.hull graph rest |> String.concat "\n")
                0
            | PrefixNoCase "inversehull" _ :: rest -> 
                printfn "%s" (Untangle.inversehull graph rest |> String.concat "\n")
                0        
            | PrefixNoCase "csvmatrix" _ :: fileName :: [] ->
                System.IO.File.WriteAllText(fileName, DependencyGraph.graphToCsv graph)
                0
            | PrefixNoCase "edgecheck" _ :: sourceRegex :: targetRegex :: [] ->
                let matches = Untangle.edgecheck graph sourceRegex targetRegex |> Array.ofSeq
                Seq.map edgeToArrow matches |> String.concat "\n" |> printfn "%s" 
                matches.Length
            | PrefixNoCase "checkcsv" _ :: fileName :: [] ->
                let disallowedEdges = Untangle.checkcsv graph fileName |> Array.ofSeq
                Seq.map edgeToArrow disallowedEdges |> String.concat "\n" |> printfn "%s" 
                disallowedEdges.Length
            | PrefixNoCase "clusterbyhulls" _:: rootNodeRegex :: dotFileName :: [] ->
                let reducedGraph = Untangle.clusterByDependentNodeCoverage graph [rootNodeRegex]
                System.IO.File.WriteAllText(dotFileName, DependencyGraph.graphToDot reducedGraph)
                0
            | _ -> 
                printfn "Could not parse command(s)."
                1

[<EntryPoint>]
let main argv = 
    let dotFileName = defaultArg (Array.tryItem 0 argv) @"D:\sw\graphs\mm.depends.dot"
    let argList = if (argv.Length < 2) then [] else argv |> List.ofSeq |> List.tail

    let fullGraph = readGraphFromDotFile dotFileName
    let filteredGraph = 
        fullGraph
        |> DependencyGraph.filterNodes (not << regexPredicate ".*Test.*")
        |> DependencyGraph.filterNodes (not << regexPredicate "sdk.*")
        |> DependencyGraph.filterNodes (not << regexPredicate ".*Companion.*")
    
    Commands.run filteredGraph (argv |> List.ofSeq |> List.skip 1)

    //let coreToCoreEdges = Untangle.edgematch filteredGraph "Core$" "Core$" |> List.ofSeq
    //printfn "%A" coreToCoreEdges

    //let reducedGraph = Untangle.clusterByDependentNodeCoverage filteredGraph ["Core$"]
    //
    ////System.IO.File.WriteAllText(@"D:\sw\graphs\mergedDepends.dot", DependencyGraph.graphToDot reducedGraph)
    //
    //System.IO.File.WriteAllText(@"D:\sw\graphs\fullMatrix.csv", DependencyGraph.graphToCsv fullGraph)
    //
    //Console.ReadLine() |> ignore
    // return an integer exit code
