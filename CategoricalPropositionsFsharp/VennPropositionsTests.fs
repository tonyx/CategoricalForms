
module catSyllogism.Tests

open NUnit.Framework

open catSyllogism.Expressions

open System

[<Test>]
    let ``A type of Venn diagram maps to Proposition "All S is P"``()=
        let allSIsP = {S=BlackFilled; SP=Empty;P=Empty}
        let expectedProposition = [{quantifier1=All;category1=S;appartenence=Is;category2=P}]
        let actual = VennToPropositions allSIsP
        Assert.AreEqual(actual,expectedProposition)

[<Test>]
    let ``A type of Venn diagram maps to Proposition "All P is S"``()=
        let allSIsP = {P=BlackFilled; SP=Empty;S=Empty}
        let expectedProposition = [{quantifier1=All;category1=P;appartenence=Is;category2=S}]
        let actual = VennToPropositions allSIsP
        Assert.AreEqual(actual,expectedProposition)

[<Test>]
    let ``A type Venn diagram reversed Order rempams to the correct order "All P Is S"``()=
        let allPIsS = {S=Empty; SP=Empty;P=BlackFilled}
        let expectedProposition = [{quantifier1=All;category1=P;appartenence=Is;category2=S}]
        let actual = VennToPropositions allPIsS
        Assert.AreEqual(actual,expectedProposition)
                    
[<Test>]
    let ``Proposition E graph has two expression formula``()=
        let noSIsP = {S=Empty; SP=BlackFilled;P=Empty}
        let expectedProposition = [{quantifier1=No;category1=S;appartenence=Is;category2=P};{quantifier1=No;category1=P;appartenence=Is;category2=S}]
        let actual = VennToPropositions noSIsP
        Assert.AreEqual(actual,expectedProposition)

[<Test>]
    let ``Proposition I graph has two expressions formulas``()=
        let someSIsP = {S=Empty; SP=Starred;P=Empty}
        let expectedProposition =  [{quantifier1=Somes;category1=S;appartenence=Is;category2=P};{quantifier1=Somes;category1=P;appartenence=Is;category2=S}]
        let actual = VennToPropositions someSIsP
        Assert.AreEqual(actual,expectedProposition)

[<Test>]
    let ``Proposition O graph has one expression formula``()=
        let someSIsNoP = {S=Starred; SP=Empty;P=Empty}
        let expectedProposition =  [{quantifier1=Somes;category1=S;appartenence=IsNot;category2=P}]
        let actual = VennToPropositions someSIsNoP
        Assert.AreEqual(actual,expectedProposition)

[<Test>]
    let ``Proposition O graph has one expression formula reversed``()=
        let somePIsNoS = {S=Empty; SP=Empty;P=Starred}
        let expectedProposition =  [{quantifier1=Somes;category1=P;appartenence=IsNot;category2=S}]
        let actual = VennToPropositions somePIsNoS
        Assert.AreEqual(actual,expectedProposition)

// Venn diagrams to propositions

[<Test>]
    let ``complex graph to propositions``()=
        let someSIsNotP_SomePIsNotS = {S=Starred; SP=Empty;P=Starred}
        let expectedProposition =  [{quantifier1=Somes;category1=S;appartenence=IsNot;category2=P};{quantifier1=Somes;category1=P;appartenence=IsNot;category2=S}]
        let actual = VennToPropositions someSIsNotP_SomePIsNotS
        Assert.AreEqual(actual,expectedProposition)

[<Test>]
    let ``complex graph to propositions 1``()=
        let vennComplexDiagram = {S=Starred; SP=BlackFilled;P=Starred}
        let NoSIsP = {quantifier1=No;category1=S;appartenence=Is;category2=P}
        let SomeSIsNoP = {quantifier1=Somes;category1=S;appartenence=IsNot;category2=P}
        let SomePIsNoS = {quantifier1=Somes;category1=P;appartenence=IsNot;category2=S}
        let NoPIisS = {quantifier1=No;category1=P;appartenence=Is;category2=S}

        let expectedProposition = List.sort [NoSIsP;SomeSIsNoP;SomePIsNoS;NoPIisS]

        let actual = List.sort (VennToPropositions vennComplexDiagram)
        Assert.AreEqual(actual,expectedProposition)

[<Test>]
    let ``complex graph to propositions 2``()=
        let vennComplexDiagram = {S=Starred; SP=Empty;P=Starred}
        let SomeSIsNoP = {quantifier1=Somes;category1=S;appartenence=IsNot;category2=P}
        let SomePIsNoS = {quantifier1=Somes;category1=P;appartenence=IsNot;category2=S}

        let expectedProposition = List.sort [SomeSIsNoP;SomePIsNoS]

        let actual = List.sort (VennToPropositions vennComplexDiagram)
        Assert.AreEqual(actual,expectedProposition)

[<Test>]
    let ``complex graph to propositions 4``()=
        let vennComplexDiagram = {S=Starred; SP=Starred;P=Starred}
        let SomeSIsNoP = {quantifier1=Somes;category1=S;appartenence=IsNot;category2=P}
        let SomeSIsP =   {quantifier1=Somes;category1=S;appartenence=Is;category2=P}
        let SomePIsS =   {quantifier1=Somes;category1=P;appartenence=Is;category2=S}
        let SomePIsNoS = {quantifier1=Somes;category1=P;appartenence=IsNot;category2=S}

        let expectedProposition = List.sort [SomeSIsNoP;SomeSIsP;SomePIsS;SomePIsNoS]

        let actual = List.sort (VennToPropositions vennComplexDiagram)
        Assert.AreEqual(actual,expectedProposition)

[<Test>]
    let ``complex graph to propositions 6``()=
        let vennComplexDiagram = {S=BlackFilled; SP=Empty;P=Starred}
        let AllSIsP = {quantifier1=All;category1=S;appartenence=Is;category2=P}
        let SomePIsNotS = {quantifier1=Somes;category1=P;appartenence=IsNot;category2=S}
        let expectedProposition = List.sort [AllSIsP;SomePIsNotS]

        let actual = List.sort (VennToPropositions vennComplexDiagram)
        Assert.AreEqual(actual,expectedProposition)

[<Test>]
    let ``complex graph to propositions 7``()=
        let vennComplexDiagram = {S=BlackFilled; SP=Empty;P=BlackFilled}
        let AllSIsP = {quantifier1=All;category1=S;appartenence=Is;category2=P}
        let AllPIsS = {quantifier1=All;category1=P;appartenence=Is;category2=S}
        let expectedProposition = List.sort [AllSIsP;AllPIsS]

        let actual = List.sort (VennToPropositions vennComplexDiagram)
        Assert.AreEqual(actual,expectedProposition)

[<Test>]
    let ``Venn diagram all filled``()=
        let vennComplexDiagram = {S=BlackFilled; SP=BlackFilled;P=BlackFilled}
        let AllSIsP = {quantifier1=All;category1=S;appartenence=Is;category2=P}
        let NoSIsP = {quantifier1=No;category1=S;appartenence=Is;category2=P}
        let AllPIsS = {quantifier1=All;category1=P;appartenence=Is;category2=S}
        let NoPIsS = {quantifier1=No;category1=P;appartenence=Is;category2=S}

        let expectedProposition =  List.sort [AllSIsP;NoSIsP;AllPIsS;NoPIsS]

        let actual = List.sort (VennToPropositions vennComplexDiagram)

        Assert.AreEqual(expectedProposition,actual)

[<Test>]
    let ``Venn diagram all empty``()=
        let vennComplexDiagram = {S=Empty; SP=Empty;P=Empty}    
        let actual = VennToPropositions vennComplexDiagram
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
         let NoSIsP = {S=Empty; SP=BlackFilled;P=Empty}
         let actual = basicCategoricalDecomposition NoSIsP
         let expected = [{S=Empty;SP=BlackFilled;P=Empty}]
         Assert.AreEqual(expected,actual)

[<Test>]
    let ``decomposed diagram of I basic diagram``()=
         let SomeSIsP = {S=Empty; SP=Starred;P=Empty}
         let actual = basicCategoricalDecomposition SomeSIsP
         let expected = [{S=Empty;SP=Starred;P=Empty}]
         Assert.AreEqual(expected,actual)

[<Test>]
    let ``decomposed diagram of O basic diagram``()=
         let SomeSIsNoP = {S=Starred; SP=Empty;P=Empty}
         let actual = basicCategoricalDecomposition SomeSIsNoP
         let expected = [{S=Starred; SP=Empty;P=Empty}]
         Assert.AreEqual(expected,actual)

[<Test>]
    let ``complex decomposition``()=
         let SomeSIsNoP = {S=Starred; SP=BlackFilled;P=Starred}
         let actual = basicCategoricalDecomposition SomeSIsNoP
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
    let ``consistents expressions``()=
        let someMIsNotP = {S=Starred; SP=Empty; P = Empty}
        let somePIsNotS = {S=Empty; SP=Empty; P = Starred}
        let canMergeExpressions = canMergeExpressions someMIsNotP somePIsNotS

        Assert.IsTrue(canMergeExpressions)

[<Test>]
    let ``A incompatible with O``()=
        let allSIsP = {S=BlackFilled; SP=Empty; P = Empty}
        let someSIsNotP = {S=Starred; SP=Empty; P = Empty}
        let canMergeExpressions = canMergeExpressions allSIsP someSIsNotP
        Assert.IsFalse(canMergeExpressions)

[<Test>]
    let ``E incompatible with I``()=
        let allEIsI = {S=Empty; SP=BlackFilled; P = Empty}
        let someSIsP = {S=Empty; SP=Starred; P = Empty}
        let canMergeExpressions = canMergeExpressions allEIsI someSIsP
        Assert.IsFalse(canMergeExpressions)

[<Test>]
[<ExpectedException>]
    let ``incompatibles with list``()=
        let allEIsI = {S=Empty; SP=BlackFilled; P = Empty}
        let someSIsP = {S=Empty; SP=Starred; P = Empty}
        let actual = mergeAll [allEIsI;someSIsP]
        Assert.IsTrue(false)


[<Test>]
    let ``incompatibles with list unmergable refactored``()=
        let allEIsI = {S=Empty; SP=BlackFilled; P = Empty}
        let someSIsP = {S=Empty; SP=Starred; P = Empty}
        let actual = mergeAllRefactored [Some allEIsI;Some someSIsP]
        //Assert.IsTrue(true)
        Assert.AreEqual(None,actual)

[<Test>]
    let ``incompatibles with list unmergable``()=
        let allEIsI = {S=Empty; SP=BlackFilled; P = Empty}
        let someSIsP = {S=Empty; SP=Starred; P = Empty}
        let actual = mergeAll [allEIsI;someSIsP]
       // Assert.IsTrue(true)
        Assert.AreEqual(None,actual)

[<Test>]
    let ``A compatible with I``()=
        let AllSIsP = {S=BlackFilled; SP=Empty; P = Empty}
        let someSIsP = {S=Empty; SP=Starred; P = Empty}
        let canMergeExpressions = canMergeExpressions AllSIsP someSIsP
        Assert.IsTrue(canMergeExpressions)


[<Test>]
    let ``A compatible with I xxx``()=
        let AllSIsP = {S=BlackFilled; SP=Empty; P = Empty}
        let someSIsP = {S=Empty; SP=Starred; P = Empty}
        let actual = mergeAll [AllSIsP;someSIsP]
        let expected = Some {S=BlackFilled;SP=Starred;P=Empty}
        Assert.AreEqual(actual,expected)

[<Test>]
    let ``A compatible with I refactoring``()=
        let AllSIsP = {S=BlackFilled; SP=Empty; P = Empty}
        let someSIsP = {S=Empty; SP=Starred; P = Empty}
        let actual = mergeAllRefactored [Some AllSIsP;Some someSIsP]
        let expected = Some {S=BlackFilled;SP=Starred;P=Empty}
        Assert.AreEqual(expected,actual)
      
[<Test>]
    let ``E compatible with O``()=
        let noSIsP = {S=Empty; SP=BlackFilled; P = Empty}
        let someSIsNotP = {S=Starred; SP=Empty; P = Empty}
        let canMergeExpressions = canMergeExpressions noSIsP someSIsNotP
        Assert.IsTrue(canMergeExpressions)


// test merge:
 
[<Test>]
    let ``quantifier merge ``()=
        let someSIsNotP = {S=Starred; SP=Empty; P = Empty}
        let somePIsNotS = {S=Empty; SP=Empty; P = Starred}
        let expectedMerge = Some {S=Starred;SP=Empty;P=Starred}
        let actualMerge = mergeAll [someSIsNotP;somePIsNotS]

        Assert.AreEqual(expectedMerge,actualMerge)

[<Test>]
    let ``quantifier merge Refactoring``()=
        let someSIsNotP = {S=Starred; SP=Empty; P = Empty}
        let somePIsNotS = {S=Empty; SP=Empty; P = Starred}
        let expectedMerge = Some {S=Starred;SP=Empty;P=Starred}
        let actualMerge = mergeAllRefactored [Some someSIsNotP;Some somePIsNotS]

        Assert.AreEqual(expectedMerge,actualMerge)

[<Test>]
    let ``quantifier merge with list``()=
        let someSIsNotP = {S=Starred; SP=Empty; P = Empty}
        let somePIsNotS = {S=Empty; SP=Empty; P = Starred}
        let expectedMerge = Some {S=Starred;SP=Empty;P=Starred}
        let actualMerge = mergeAll [someSIsNotP ;somePIsNotS]

        Assert.AreEqual(expectedMerge,actualMerge)

[<Test>]
    let ``quantifier merge with list Refactored``()=
        let someSIsNotP = {S=Starred; SP=Empty; P = Empty}
        let somePIsNotS = {S=Empty; SP=Empty; P = Starred}
        let expectedMerge = Some {S=Starred;SP=Empty;P=Starred}
        let actualMerge = mergeAllRefactored [Some someSIsNotP ;Some somePIsNotS]

        Assert.AreEqual(actualMerge,expectedMerge)

[<Test>]
    let ``quantifier merge 1``()=
        let someSIsNotP = {S=Starred; SP=Empty; P = Empty}
        let allPIsS = {S=Empty; SP=Empty; P = BlackFilled}
        let expectedMerge = Some {S=Starred;SP=Empty;P=BlackFilled}
        let actualMerge = mergeAll [someSIsNotP;allPIsS]

        Assert.AreEqual(expectedMerge,actualMerge)

[<Test>]
    let ``quantifier merge 1 Refactored``()=
        let someSIsNotP = {S=Starred; SP=Empty; P = Empty}
        let allPIsS = {S=Empty; SP=Empty; P = BlackFilled}
        let expectedMerge = Some {S=Starred;SP=Empty;P=BlackFilled}
        let actualMerge = mergeAllRefactored [Some someSIsNotP;Some allPIsS]

        Assert.AreEqual(expectedMerge,actualMerge)

[<Test>]
    let ``quantifier merge 2``()=
        let someSIsNotP = {S=Starred; SP=Empty; P = Empty}
        let allPIsS = {S=Empty; SP=Empty; P = BlackFilled}
        let expectedMerge = Some {S=Starred;SP=Empty;P=BlackFilled}
        let actualMerge = mergeAll [someSIsNotP;allPIsS]

        Assert.AreEqual(expectedMerge,actualMerge)


[<Test>]
    let ``quantifier merge 2 Refactored``()=
        let someSIsNotP = {S=Starred; SP=Empty; P = Empty}
        let allPIsS = {S=Empty; SP=Empty; P = BlackFilled}
        let expectedMerge = Some {S=Starred;SP=Empty;P=BlackFilled}
        let actualMerge = mergeAllRefactored [Some someSIsNotP;Some allPIsS]

        Assert.AreEqual(expectedMerge,actualMerge)




                 
