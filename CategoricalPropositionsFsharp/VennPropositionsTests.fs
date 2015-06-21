
module catSyllogism.Tests

open NUnit.Framework

open catSyllogism.Expressions

open System

[<Test>]
    let ``A type of Venn diagram maps to Proposition "All S is P"``()=
        let allSIsP = {S=BlackFilled; SP=Empty;P=Empty}
        let expectedProposition = [{quantifier1=All;category1=S;appartenence=Is;category2=P}]
        let actual = vennToPropositions allSIsP
        Assert.AreEqual(expectedProposition,actual)

[<Test>]
    let ``A type of Venn diagram maps to Proposition "All P is S"``()=
        let allSIsP = {P=BlackFilled; SP=Empty;S=Empty}
        let expectedProposition = [{quantifier1=All;category1=P;appartenence=Is;category2=S}]
        let actual = vennToPropositions allSIsP
        Assert.AreEqual(expectedProposition,actual)

[<Test>]
    let ``A type Venn diagram reversed Order rempams to the correct order "All P Is S"``()=
        let allPIsS = {S=Empty; SP=Empty;P=BlackFilled}
        let expectedProposition = [{quantifier1=All;category1=P;appartenence=Is;category2=S}]
        let actual = vennToPropositions allPIsS
        Assert.AreEqual(expectedProposition,actual)
                    
[<Test>]
    let ``Proposition E graph has two expression formula``()=
        let noSIsP = {S=Empty; SP=BlackFilled;P=Empty}
        let expectedProposition = [{quantifier1=No;category1=S;appartenence=Is;category2=P};{quantifier1=No;category1=P;appartenence=Is;category2=S}]
        let actual = vennToPropositions noSIsP
        Assert.AreEqual(expectedProposition,actual)

[<Test>]
    let ``Proposition I graph has two expressions formulas``()=
        let someSIsP = {S=Empty; SP=Starred;P=Empty}
        let expectedProposition =  [{quantifier1=Somes;category1=S;appartenence=Is;category2=P};{quantifier1=Somes;category1=P;appartenence=Is;category2=S}]
        let actual = vennToPropositions someSIsP
        Assert.AreEqual(expectedProposition,actual)

[<Test>]
    let ``Proposition O graph has one expression formula``()=
        let someSIsNoP = {S=Starred; SP=Empty;P=Empty}
        let expectedProposition =  [{quantifier1=Somes;category1=S;appartenence=IsNot;category2=P}]
        let actual = vennToPropositions someSIsNoP
        Assert.AreEqual(expectedProposition,actual)

[<Test>]
    let ``Proposition O graph has one expression formula reversed``()=
        let somePIsNoS = {S=Empty; SP=Empty;P=Starred}
        let expectedProposition =  [{quantifier1=Somes;category1=P;appartenence=IsNot;category2=S}]
        let actual = vennToPropositions somePIsNoS
        Assert.AreEqual(expectedProposition,actual)

// Venn diagrams to propositions

[<Test>]
    let ``complex graph to propositions``()=
        let someSIsNotP_SomePIsNotS = {S=Starred; SP=Empty;P=Starred}
        let expectedProposition =  [{quantifier1=Somes;category1=S;appartenence=IsNot;category2=P};{quantifier1=Somes;category1=P;appartenence=IsNot;category2=S}]
        let actual = vennToPropositions someSIsNotP_SomePIsNotS
        Assert.AreEqual(expectedProposition,actual)

[<Test>]
    let ``complex graph to propositions 1``()=
        let vennComplexDiagram = {S=Starred; SP=BlackFilled;P=Starred}
        let noSIsP = {quantifier1=No;category1=S;appartenence=Is;category2=P}
        let someSIsNoP = {quantifier1=Somes;category1=S;appartenence=IsNot;category2=P}
        let somePIsNoS = {quantifier1=Somes;category1=P;appartenence=IsNot;category2=S}
        let noPIsS = {quantifier1=No;category1=P;appartenence=Is;category2=S}

        let expectedProposition = List.sort [noSIsP;someSIsNoP;somePIsNoS;noPIsS]

        let actual = List.sort (vennToPropositions vennComplexDiagram)
        Assert.AreEqual(expectedProposition,actual)

[<Test>]
    let ``complex graph to propositions 2``()=
        let vennComplexDiagram = {S=Starred; SP=Empty;P=Starred}
        let someSIsNoP = {quantifier1=Somes;category1=S;appartenence=IsNot;category2=P}
        let somePIsNoS = {quantifier1=Somes;category1=P;appartenence=IsNot;category2=S}

        let expectedProposition = List.sort [someSIsNoP;somePIsNoS]

        let actual = List.sort (vennToPropositions vennComplexDiagram)
        Assert.AreEqual(expectedProposition,actual)

[<Test>]
    let ``complex graph to propositions 4``()=
        let vennComplexDiagram = {S=Starred; SP=Starred;P=Starred}
        let someSIsNoP = {quantifier1=Somes;category1=S;appartenence=IsNot;category2=P}
        let someSIsP =   {quantifier1=Somes;category1=S;appartenence=Is;category2=P}
        let somePIsS =   {quantifier1=Somes;category1=P;appartenence=Is;category2=S}
        let somePIsNoS = {quantifier1=Somes;category1=P;appartenence=IsNot;category2=S}

        let expectedProposition = List.sort [someSIsNoP;someSIsP;somePIsS;somePIsNoS]

        let actual = List.sort (vennToPropositions vennComplexDiagram)
        Assert.AreEqual(expectedProposition,actual)

[<Test>]
    let ``complex graph to propositions 6``()=
        let vennComplexDiagram = {S=BlackFilled; SP=Empty;P=Starred}
        let allSIsP = {quantifier1=All;category1=S;appartenence=Is;category2=P}
        let SomePIsNotS = {quantifier1=Somes;category1=P;appartenence=IsNot;category2=S}
        let expectedProposition = List.sort [allSIsP;SomePIsNotS]

        let actual = List.sort (vennToPropositions vennComplexDiagram)
        Assert.AreEqual(expectedProposition,actual)

[<Test>]
    let ``complex graph to propositions 7``()=
        let vennComplexDiagram = {S=BlackFilled; SP=Empty;P=BlackFilled}
        let allSIsP = {quantifier1=All;category1=S;appartenence=Is;category2=P}
        let allPIsS = {quantifier1=All;category1=P;appartenence=Is;category2=S}
        let expectedProposition = List.sort [allSIsP;allPIsS]

        let actual = List.sort (vennToPropositions vennComplexDiagram)
        Assert.AreEqual(expectedProposition,actual)

[<Test>]
    let ``complex graph to propositions 8``()=
        let vennComplexDiagram = {S=Starred; SP=BlackFilled;P=Starred}

        let someSIsNotP = {quantifier1=Somes;category1=S;appartenence=IsNot;category2=P}
        let noPIsS = {quantifier1=No;category1=P;appartenence=Is;category2=S}
        let noSIsP = {quantifier1=No;category1=S;appartenence=Is;category2=P}
        let somePIsNotS = {quantifier1=Somes;category1=P;appartenence=IsNot;category2=S}

        let expectedProposition = List.sort [someSIsNotP;noSIsP;noPIsS;somePIsNotS]

        let actual = List.sort (vennToPropositions vennComplexDiagram)
        Assert.AreEqual(expectedProposition,actual)



[<Test>]
    let ``Venn diagram all filled``()=
        let vennComplexDiagram = {S=BlackFilled; SP=BlackFilled;P=BlackFilled}
        let allSIsP = {quantifier1=All;category1=S;appartenence=Is;category2=P}
        let noSIsP = {quantifier1=No;category1=S;appartenence=Is;category2=P}
        let allPIsS = {quantifier1=All;category1=P;appartenence=Is;category2=S}
        let noPIsS = {quantifier1=No;category1=P;appartenence=Is;category2=S}

        let expectedProposition =  List.sort [allSIsP;noSIsP;allPIsS;noPIsS]

        let actual = List.sort (vennToPropositions vennComplexDiagram)

        Assert.AreEqual(expectedProposition,actual)

[<Test>]
    let ``Venn diagram all empty``()=
        let vennComplexDiagram = {S=Empty; SP=Empty;P=Empty}    
        let actual = vennToPropositions vennComplexDiagram
        Assert.IsTrue((actual = []))


// decomposition in basic venn forms (A E I O)
[<Test>] 
    let ``decomposed diagram of A basic diagram``()=
         let allSIsP = {S=BlackFilled; SP=Empty;P=Empty}
         let actual = basicCategoricalDecomposition allSIsP
         let expected = [{S=BlackFilled;SP=Empty;P=Empty}]
         Assert.AreEqual(expected,actual)

[<Test>] 
    let ``decomposed diagram of E basic diagram``()=
         let noSIsP = {S=Empty; SP=BlackFilled;P=Empty}
         let actual = basicCategoricalDecomposition noSIsP
         let expected = [{S=Empty;SP=BlackFilled;P=Empty}]
         Assert.AreEqual(expected,actual)

[<Test>]
    let ``decomposed diagram of I basic diagram``()=
         let someSIsP = {S=Empty; SP=Starred;P=Empty}
         let actual = basicCategoricalDecomposition someSIsP
         let expected = [{S=Empty;SP=Starred;P=Empty}]
         Assert.AreEqual(expected,actual)

[<Test>]
    let ``decomposed diagram of O basic diagram``()=
         let someSIsNoP = {S=Starred; SP=Empty;P=Empty}
         let actual = basicCategoricalDecomposition someSIsNoP
         let expected = [{S=Starred; SP=Empty;P=Empty}]
         Assert.AreEqual(expected,actual)

[<Test>]
    let ``complex decomposition``()=
         let someSIsNoP = {S=Starred; SP=BlackFilled;P=Starred}
         let actual = basicCategoricalDecomposition someSIsNoP
         let expected = [{S=Starred;SP=Empty;P=Empty};{S=Empty;SP=BlackFilled;P=Empty};{S=Empty;SP=Empty;P=Starred}]
         Assert.AreEqual(expected,actual)

[<Test>]
    let ``composed example 2``()=
        let composedExample = {S=Starred;SP=Empty;P=BlackFilled}
        let expected = [{S=Starred;SP=Empty;P=Empty};{S=Empty;SP=Empty;P=BlackFilled}]
        let actual = basicCategoricalDecomposition composedExample
        Assert.AreEqual(expected,actual)

[<Test>]
    let ``composed example 2.1``()=
        let composedExample = {S=BlackFilled;SP=Starred;P=Starred}
        let expected = [{S=BlackFilled;SP=Empty;P=Empty};{S=Empty;SP=Starred;P=Empty};{S=Empty;SP=Empty;P=Starred}]
        let actual = basicCategoricalDecomposition composedExample
        Assert.AreEqual(expected,actual)

[<Test>]
    let ``composed example 3``()=
        let composedExample = {S=Starred;SP=Empty;P=Starred}
        let expected = [{S=Starred;SP=Empty;P=Empty};{S=Empty;SP=Empty;P=Starred}]
        let actual = basicCategoricalDecomposition composedExample
        Assert.AreEqual(expected,actual)

[<Test>]
    let ``composed example 4``()=
        let composedExample = {S=Starred;SP=Starred;P=Starred}
        let expected = [{S=Starred;SP=Empty;P=Empty};{S=Empty;SP=Starred;P=Empty};{S=Empty;SP=Empty;P=Starred}]
        let actual = basicCategoricalDecomposition composedExample
        Assert.AreEqual(expected,actual)



[<Test>]
    let ``A incompatible with O``()=
        let allSIsP =  {S=BlackFilled; SP=Empty; P = Empty}
        let someSIsNotP =  {S=Starred; SP=Empty; P = Empty}
        let canMergeExpressions = merge allSIsP  someSIsNotP
        Assert.AreEqual(None,canMergeExpressions)



[<Test>]
    let ``incompatibles with list unmergable``()=
        let allEIsI = {S=Empty; SP=BlackFilled; P = Empty}
        let someSIsP = {S=Empty; SP=Starred; P = Empty}
        let actual = mergeAll [ allEIsI; someSIsP]
        Assert.AreEqual(None,actual)



[<Test>]
    let ``A compatible with I ``()=
        let allSIsP = {S=BlackFilled; SP=Empty; P = Empty}
        let someSIsP = {S=Empty; SP=Starred; P = Empty}
        let actual = mergeAll [ allSIsP; someSIsP]
        let expected = Some {S=BlackFilled;SP=Starred;P=Empty}
        Assert.AreEqual(expected,actual)
      
 

[<Test>]
    let ``quantifier merge ``()=
        let someSIsNotP = {S=Starred; SP=Empty; P = Empty}
        let somePIsNotS = {S=Empty; SP=Empty; P = Starred}
        let expectedMerge = Some {S=Starred;SP=Empty;P=Starred}
        let actualMerge = mergeAll [someSIsNotP;somePIsNotS]

        Assert.AreEqual(expectedMerge,actualMerge)


[<Test>]
    let ``quantifier merge with list ``()=
        let someSIsNotP = {S=Starred; SP=Empty; P = Empty}
        let somePIsNotS = {S=Empty; SP=Empty; P = Starred}
        let expectedMerge = Some {S=Starred;SP=Empty;P=Starred}
        let actualMerge = mergeAll [ someSIsNotP ; somePIsNotS]

        Assert.AreEqual(actualMerge,expectedMerge)


[<Test>]
    let ``quantifier merge 1 ``()=
        let someSIsNotP = {S=Starred; SP=Empty; P = Empty}
        let allPIsS = {S=Empty; SP=Empty; P = BlackFilled}
        let expectedMerge = Some {S=Starred;SP=Empty;P=BlackFilled}
        let actualMerge = mergeAll [ someSIsNotP; allPIsS]

        Assert.AreEqual(expectedMerge,actualMerge)



[<Test>]
    let ``quantifier merge 2 ``()=
        let someSIsNotP = {S=Starred; SP=Empty; P = Empty}
        let allPIsS = {S=Empty; SP=Empty; P = BlackFilled}
        let expectedMerge = Some {S=Starred;SP=Empty;P=BlackFilled}
        let actualMerge = mergeAll [ someSIsNotP; allPIsS]

        Assert.AreEqual(expectedMerge,actualMerge)


[<Test>]
    let ``complex merge test ``()=
        let someSIsP = {S=Empty; SP=Starred; P = Empty}
        let someSIsNotP = {S=Starred; SP=Empty; P = Empty}
        let expectedMerge = Some {S=Starred;SP=Starred;P=Empty}
        let actualMerge = mergeAll [ someSIsNotP; someSIsP]

        Assert.AreEqual(expectedMerge,actualMerge)
                 
[<Test>]
    let ``complex merge test 2 ``()=
        let someSIsP = {S=Empty; SP=Starred; P = Empty}
        let someSIsNotP = {S=Starred; SP=Empty; P = Empty}
        let somePIsNotS = {S=Empty; SP=Empty; P = Starred}
        let expectedMerge = Some {S=Starred;SP=Starred;P=Starred}
        let actualMerge = mergeAll [ someSIsNotP; someSIsP;somePIsNotS]

        Assert.AreEqual(expectedMerge,actualMerge)
                 
        // Some S is not P, No S is P, No P is S, and Some P is not S.
[<Test>]
    let ``complex merge test 3 ``()=
        let someSIsNotP = {S=Starred; SP=Empty; P = Empty}
        let noSIsP = {S=Empty; SP=BlackFilled; P = Empty}
        let noPIsS = {S=Empty; SP=BlackFilled; P = Empty}
        let somePIssNotS = {S=Empty; SP=Empty; P = Starred}

        let expectedMerge = Some {S=Starred;SP=BlackFilled;P=Starred}
        let actualMerge = mergeAll [ someSIsNotP; noSIsP;somePIssNotS]

        Assert.AreEqual(expectedMerge,actualMerge)

[<Test>]
    let ``merge two equals elements gets the same element back``()=
        let noSIsP = {S=Empty; SP=BlackFilled; P = Empty}
        let noPIsS = {S=Empty; SP=BlackFilled; P = Empty}
        let expected = Some noSIsP
        let actualMerge = mergeAll [noSIsP;noPIsS]
        Assert.AreEqual(expected ,actualMerge)

