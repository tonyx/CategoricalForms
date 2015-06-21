module catSyllogism.PropositionToVennTests

open NUnit.Framework

open catSyllogism.Expressions

[<Test>]
    let ``All S is P``()=
        let allSIsPProposition = {quantifier1=All;category1=S;appartenence=Is;category2=P}
        let allSIsPDiagram = Some {S=BlackFilled;SP=Empty;P=Empty}

        let actual = singlePropositionToVenn allSIsPProposition 
        Assert.AreEqual(allSIsPDiagram, actual)

[<Test>]
    let ``All P is S``()=
        let allPIsSProposition = {quantifier1=All;category1=P;appartenence=Is;category2=S}
        let allPIsSDiagram = Some {S=Empty;SP=Empty;P=BlackFilled}

        let actual = singlePropositionToVenn allPIsSProposition 
        Assert.AreEqual(allPIsSDiagram, actual)


[<Test>]
    let ``No S is P``()=
        let noSIsPProposition = {quantifier1=No;category1=S;appartenence=Is;category2=P}
        let noSIsPDiagram = Some {S=Empty;SP=BlackFilled;P=Empty}

        let actual = singlePropositionToVenn noSIsPProposition 
        Assert.AreEqual(noSIsPDiagram,actual)

[<Test>]
    let ``No P is S``()=
        let noSIsPProposition = {quantifier1=No;category1=P;appartenence=Is;category2=S}
        let noSIsPDiagram = Some {S=Empty;SP=BlackFilled;P=Empty}

        let actual = singlePropositionToVenn noSIsPProposition 
        Assert.AreEqual(noSIsPDiagram,actual)

[<Test>]
    let ``Some S is P``()=
        let someSIsPProposition = {quantifier1=Somes;category1=S;appartenence=Is;category2=P}
        let someSIsPDiagram = Some {S=Empty;SP=Starred;P=Empty}

        let actual = singlePropositionToVenn someSIsPProposition
        Assert.AreEqual(someSIsPDiagram,actual)

[<Test>]
    let ``Some P is S``()=
        let someSIsPProposition = {quantifier1=Somes;category1=P;appartenence=Is;category2=S}
        let someSIsPDiagram = Some {S=Empty;SP=Starred;P=Empty}

        let actual = singlePropositionToVenn someSIsPProposition
        Assert.AreEqual(someSIsPDiagram,actual)

[<Test>]
    let ``Some S is not P``()=
        let someSIsNotPProposition = {quantifier1=Somes;category1=S;appartenence=IsNot;category2=P}
        let someSIsNotPDiagram = Some {S=Starred;SP=Empty;P=Empty}

        let actual = singlePropositionToVenn someSIsNotPProposition
        Assert.AreEqual(someSIsNotPDiagram,actual)

[<Test>]
    let ``Some P is not S``()=
        let somePIsNotSProposition = {quantifier1=Somes;category1=P;appartenence=IsNot;category2=S}
        let somePIsNotSDiagram = Some {S=Empty;SP=Empty;P=Starred}

        let actual = singlePropositionToVenn somePIsNotSProposition
        Assert.AreEqual(somePIsNotSDiagram,actual)

[<Test>]
    let ``contraddiction: No P is P``()=
        let NoPIsPProposition = {quantifier1=No;category1=P;appartenence=Is;category2=P}
        let noPIsPDiagram = None

        let actual = singlePropositionToVenn NoPIsPProposition
        Assert.AreEqual(noPIsPDiagram,actual)

[<Test>]
    let ``thautologic will get Empty: All P is P``()=
        let allPIsPProposition = {quantifier1=All;category1=P;appartenence=Is;category2=P}
        let allPIsPDiagram = Some {S=Empty;SP=Empty;P=Empty}

        let actual = singlePropositionToVenn allPIsPProposition
        Assert.AreEqual(allPIsPDiagram,actual)


[<Test>]
   let ``tautologic will get empty: All S is S``()=
        let allPIsPProposition = {quantifier1=All;category1=S;appartenence=Is;category2=S}
        let allPIsPDiagram = Some {S=Empty;SP=Empty;P=Empty}

        let actual = singlePropositionToVenn allPIsPProposition
        Assert.AreEqual(allPIsPDiagram,actual)

[<Test>]
   let ``tautologic will get empty: Some S is S``()=
        let allPIsPProposition = {quantifier1=Somes;category1=S;appartenence=Is;category2=S}
        let allPIsPDiagram = Some {S=Empty;SP=Empty;P=Empty}

        let actual = singlePropositionToVenn allPIsPProposition
        Assert.AreEqual(allPIsPDiagram,actual)


[<Test>]
   let ``thautologic: "No P isNot P"``()=
        let noPIsNotPProposition = {quantifier1=No;category1=P;appartenence=IsNot;category2=P}
        let noPIsNotPDiagram = Some {S=Empty;SP=Empty;P=Empty}

        let actual = singlePropositionToVenn noPIsNotPProposition
        Assert.AreEqual(noPIsNotPDiagram,actual)  


 [<Test>]
   let ``synonim: "No S isNot P" equivalent to "All S is P"``()=
        let noPIsNotSProposition = {quantifier1=No;category1=S;appartenence=IsNot;category2=P}
        let noPIsNotSDiagram = Some {S=BlackFilled;SP=Empty;P=Empty}

        let actual = singlePropositionToVenn  noPIsNotSProposition
        Assert.AreEqual(noPIsNotSDiagram,actual)  

 [<Test>]
   let ``synonim: "No P isNot S" equivalent to "All P is S"``()=
        let noPIsNotSProposition = {quantifier1=No;category1=P;appartenence=IsNot;category2=S}
        let noPIsNotSDiagram = Some {S=Empty;SP=Empty;P=BlackFilled}

        let actual = singlePropositionToVenn  noPIsNotSProposition
        Assert.AreEqual(noPIsNotSDiagram,actual)  


// Some S is not P, No S is P, No P is S, and Some P is not S
 [<Test>]
   let ``multiple and merged propositions``()=
        let someSIsNotPdiagram = match singlePropositionToVenn {quantifier1=Somes;category1=S;appartenence=IsNot;category2=P} with | Some X -> X
        let noSIsPDiagram = match singlePropositionToVenn {quantifier1=No;category1=S;appartenence=Is;category2=P} with | Some X -> X
        let noPIsSDiagram = match singlePropositionToVenn {quantifier1=No;category1=P;appartenence=Is;category2=S} with | Some X -> X
        let somePIsNotS = match singlePropositionToVenn {quantifier1=Somes;category1=P;appartenence=IsNot;category2=S} with | Some X -> X

        let finalDiagram = mergeAll [someSIsNotPdiagram;noSIsPDiagram;noPIsSDiagram;somePIsNotS]

        let expected = Some {S=Starred;SP=BlackFilled;P=Starred}

        Assert.AreEqual(expected,finalDiagram)  



// [<Test>]
//   let ``multiple and merged propositions complete``()=
//
//        let someSIsNotPdiagram = match singlePropositionToVenn {quantifier1=Somes;category1=S;appartenence=IsNot;category2=P} with | Some X -> X
//        let noSIsPDiagram = match singlePropositionToVenn {quantifier1=No;category1=S;appartenence=Is;category2=P} with | Some X -> X
//        let noPIsSDiagram = match singlePropositionToVenn {quantifier1=No;category1=P;appartenence=Is;category2=S} with | Some X -> X
//        let somePIsNotS = match singlePropositionToVenn {quantifier1=Somes;category1=P;appartenence=IsNot;category2=S} with | Some X -> X
//
//        let finalDiagram = mergeAll [someSIsNotPdiagram;noSIsPDiagram;noPIsSDiagram;somePIsNotS]
//
//        let finalDiagram =propositionsToVenn [
//            {quantifier1=Somes;category1=S;appartenence=IsNot;category2=P} ;
//            {quantifier1=No;category1=S;appartenence=Is;category2=P}; 
//            {quantifier1=No;category1=P;appartenence=Is;category2=S};
//            {quantifier1=Somes;category1=P;appartenence=IsNot;category2=S} 
//        
//        ]
//
//
//        let expected = Some {S=Starred;SP=BlackFilled;P=Starred}
//
//        Assert.AreEqual(expected,finalDiagram)  
//
