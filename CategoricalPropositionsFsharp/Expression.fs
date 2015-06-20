module catSyllogism.Expressions

open NUnit.Framework

type CategoryRef = S | M | P | Z
        
type Appartenence = Is | IsNot
type Quantifier = All | Somes | No

type SectionStatus = Empty | BlackFilled | Starred

type twoSetsVennDiagram = {S: SectionStatus; SP: SectionStatus; P: SectionStatus}
type twoTermsProposition = {quantifier1: Quantifier; category1: CategoryRef; appartenence: Appartenence; category2: CategoryRef}



let overlappable status1 status2 =
    match (status1,status2) with
        | (Empty,_) -> true
        | (_,Empty) -> true
        | (Starred,Starred) -> true  
        | (BlackFilled,BlackFilled) -> true
        | _ -> false

let overlapSingle s1 s2 =
    match (s1,s2) with
        | (Empty,Empty) -> Empty
        | (Empty,s2) -> s2
        | (s1,Empty) -> s1
        | _ -> failwith "non overlappable states "

let overlapSingleRefactored s1 s2 =
    match (s1,s2) with
        | (Empty,Empty) -> Some Empty
        | (Empty,s2) -> Some s2
        | (s1,Empty) -> Some s1
        | _ -> None

let canMergeExpressionsRefactored first second =   
    match (first,second) with
        | ({S=s1;SP=sp1;P=p1},{S=s2;SP=sp2;P=p2}) -> overlappable s1 s2 && overlappable sp1 sp2 && overlappable p1 p2

let canMergeExpressions first second =   
    match (first,second) with
        | ({S=s1;SP=sp1;P=p1},{S=s2;SP=sp2;P=p2}) -> overlappable s1 s2 && overlappable sp1 sp2 && overlappable p1 p2
    
let merge first second =
    let precondition = match (canMergeExpressions first second) with | true -> true | false -> failwith "unmergeable"
    match (first,second) with
        | ({S=s1;SP=sp1;P=p1},{S=s2;SP=sp2;P=p2}) -> {S=overlapSingle s1 s2; SP = overlapSingle sp1 sp2; P = overlapSingle p1 p2}


let mergeRefactored  first second = 
    let extractFirst = match first with Some X -> X | None -> {S=Empty; SP=Empty; P=Empty}
    let extractSecond = match second with Some X -> X | None ->  {S=Empty; SP=Empty; P=Empty}
    if (canMergeExpressions  extractFirst extractSecond ) then Some (merge extractFirst extractSecond) else None

       
     
type MaybeBuilder() =

    member this.Bind(x, f) = 
        match x with
        | None -> None
        | Some a -> f a

    member this.Return(x) = 
        Some x
   
let maybe = new MaybeBuilder()


       
let mergeAll toBeMerged =
    try 
        Some (List.fold (fun item acc -> merge item acc) {S=Empty;SP=Empty;P=Empty} toBeMerged)
    with
    | :? System.Exception -> None


let mergeAllRefactored toBeMerged =
    maybe 
        {
            let! toRet =  (List.fold (fun item acc -> mergeRefactored item acc) (Some {S=Empty;SP=Empty;P=Empty}) toBeMerged)
            return toRet
        }


let basicCategoricalDecomposition diagram =
        match diagram with
            | {S=s_pattern; SP=Empty;P=Empty}  when s_pattern <> Empty -> [{S=s_pattern; SP=Empty;P=Empty}] // A or O
            | {S=Empty; SP=sp_pattern;P=Empty} when sp_pattern <> Empty -> [{S=Empty; SP=sp_pattern;P=Empty}]    // E or I  
            | {S=Empty; SP=Empty;P=p_pattern} when p_pattern <> Empty -> [{S=Empty; SP=Empty;P=p_pattern}]
            | {S=s_pattern; SP=sp_pattern;P=Empty} -> [{S=s_pattern; SP=Empty; P=Empty};{S=Empty;SP=sp_pattern;P=Empty}]
            | {S=Empty; SP=sp_pattern;P=p_pattern} -> [{S=Empty; SP=sp_pattern; P=Empty};{S=Empty;SP=Empty;P=p_pattern}]
            | {S=s_pattern; SP=Empty;P=p_pattern} -> [{S=s_pattern; SP=Empty; P=Empty};{S=Empty;SP=Empty;P=p_pattern}]
            | {S=s_pattern; SP=sp_pattern;P=p_pattern} -> [{S=s_pattern;SP=Empty;P=Empty};{S=Empty;SP=sp_pattern;P=Empty};{S=Empty;SP=Empty;P=p_pattern}]
            | _ -> []


let rec VennToPropositions diagram =
    match diagram with 
        | {S=Empty; SP=Empty;P=Empty} -> []
        | {S=BlackFilled; SP=Empty;P=Empty} ->  [{quantifier1=All;category1=S;appartenence=Is;category2=P}]
        | {S=Empty; SP=Empty;P=BlackFilled} ->  [{quantifier1=All;category1=P;appartenence=Is;category2=S}]
        | {S=Empty; SP=BlackFilled;P=Empty} ->  [{quantifier1=No;category1=S;appartenence=Is;category2=P};{quantifier1=No;category1=P;appartenence=Is;category2=S}]
        | {S=Empty; SP=Starred;P=Empty} -> [{quantifier1=Somes;category1=S;appartenence=Is;category2=P};{quantifier1=Somes;category1=P;appartenence=Is;category2=S}]
        | {S=Starred; SP=Empty;P=Empty} -> [{quantifier1=Somes;category1=S;appartenence=IsNot;category2=P}]
        | {S=Empty; SP=Empty;P=Starred} -> [{quantifier1=Somes;category1=P;appartenence=IsNot;category2=S}]
        | _ -> List.fold (fun acc item -> acc @ (VennToPropositions item) ) [] (basicCategoricalDecomposition diagram)




