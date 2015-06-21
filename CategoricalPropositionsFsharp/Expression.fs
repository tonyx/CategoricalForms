module catSyllogism.Expressions

open NUnit.Framework

type CategoryRef = S | M | P | Z
        
type Appartenence = Is | IsNot
type Quantifier = All | Somes | No

type SectionStatus = Empty | BlackFilled | Starred

type twoSetsVennDiagram = {S: SectionStatus; SP: SectionStatus; P: SectionStatus}
type twoTermsProposition = {quantifier1: Quantifier; category1: CategoryRef; appartenence: Appartenence; category2: CategoryRef}


type MaybeBuilder() =
    member this.Bind(x, f) = 
        match x with
        | None -> None
        | Some a -> f a

    member this.Return(x) = 
        Some x
   
let maybe = new MaybeBuilder()

let merge first second =
    let overlap s1 s2 =
        match (s1,s2) with
            | (Empty,Empty) -> Some Empty
            | (Empty,s2) -> Some s2
            | (s1,Empty) -> Some s1
            | _ -> None

    let {S=s1;SP=sp1;P=p1}= first
    let {S=s2;SP=sp2;P=p2}= second

    maybe 
        {
            let! s = overlap s1 s2
            let! sp = overlap sp1 sp2
            let! p = overlap p1 p2
            return {S=s;SP=sp;P=p}
        }


let mergeAll toBeMerged =
            (List.fold (fun item acc -> match item with | Some X -> merge X acc | None -> None) (Some {S=Empty;SP=Empty;P=Empty}) toBeMerged)


let singlePropositionToVenn proposition =  
    match proposition with
        | {quantifier1=All;category1=S;appartenence=Is;category2=P} -> {S=BlackFilled;SP=Empty;P=Empty} 
        | {quantifier1=No;category1=S;appartenence=Is;category2=P} -> {S=Empty;SP=BlackFilled;P=Empty} 
        | {quantifier1=No;category1=P;appartenence=Is;category2=S} -> {S=Empty;SP=BlackFilled;P=Empty} 
        | {quantifier1=Somes;category1=S;appartenence=Is;category2=P} -> {S=Empty;SP=Starred;P=Empty} 
        | {quantifier1=Somes;category1=P;appartenence=Is;category2=S} -> {S=Empty;SP=Starred;P=Empty} 
        | {quantifier1=Somes;category1=S;appartenence=IsNot;category2=P} -> {S=Starred;SP=Empty;P=Empty} 



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


let rec vennToPropositions diagram =
    match diagram with 
        | {S=Empty; SP=Empty;P=Empty} -> []
        | {S=BlackFilled; SP=Empty;P=Empty} ->  [{quantifier1=All;category1=S;appartenence=Is;category2=P}]
        | {S=Empty; SP=Empty;P=BlackFilled} ->  [{quantifier1=All;category1=P;appartenence=Is;category2=S}]
        | {S=Empty; SP=BlackFilled;P=Empty} ->  [{quantifier1=No;category1=S;appartenence=Is;category2=P};{quantifier1=No;category1=P;appartenence=Is;category2=S}]
        | {S=Empty; SP=Starred;P=Empty} -> [{quantifier1=Somes;category1=S;appartenence=Is;category2=P};{quantifier1=Somes;category1=P;appartenence=Is;category2=S}]
        | {S=Starred; SP=Empty;P=Empty} -> [{quantifier1=Somes;category1=S;appartenence=IsNot;category2=P}]
        | {S=Empty; SP=Empty;P=Starred} -> [{quantifier1=Somes;category1=P;appartenence=IsNot;category2=S}]
        | _ -> List.fold (fun acc item -> acc @ (vennToPropositions item) ) [] (basicCategoricalDecomposition diagram)




