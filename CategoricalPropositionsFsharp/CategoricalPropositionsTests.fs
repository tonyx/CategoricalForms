module catSyllogism.CategoricalTests


open NUnit.Framework

open catSyllogism.Expressions

open System


[<Test>]
    let ``A type of Venn diagram maps to Proposition "All S is P"``()=
        let allSIsP = {S=BlackFilled; SP=Empty;P=Empty}
        let expectedProposition = [{quantifier1=All;category1=S;appartenence=Is;category2=P}]
        let actual = vennToPropositions allSIsP
        Assert.AreEqual(actual,expectedProposition)

