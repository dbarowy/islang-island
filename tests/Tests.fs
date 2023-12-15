namespace tests

open Parser
open Evaluator
open AST
open EvalLandscape
open System.IO
open Microsoft.VisualStudio.TestTools.UnitTesting

[<TestClass>]
type TestClass () =
    //Not reading in the file properly:
    let testProjectPath = "../../../../tests"
    //printfn "%s" testProjectPath
    let path = Path.Combine(testProjectPath, "TestData", "simple.island")
    let input = File.ReadAllText path
    //printfn "%s" path
    let do_debug = false

    [<TestMethod>]
    //Parser Test:
    member this.parseTest () =
        let ast_may = parse input do_debug
        let expected_firstline = "Some(Canvas"
        let expected_secondline = """[{ name = "TwoCircles" """
        let expected_thirdline = "dims = { w = 50"
        let expected_fourthline = "h = 50 }"
        let expected_fifthline = "components = [Circle ({ x = 5"
        let expected_sixthline = "y = 5 }, 5); Circle ({ x = 5"
        let expected_seventhline = "y = 5 }, 3)] };"
        let expected_eighthline = """{ name = "ThreeCircles" """
        let expected_ninthline = "dims = { w = 100"
        let expected_tenthline = "h = 100 }"
        let expected_eleventhline = "components ="
        let expected_twelfthline = """ [Name ("TwoCircles", RelativePlacement (Position (Right, 2), 90));"""
        let expected_thirteenthline = "Circle ({ x = 10"
        let expected_lastline = "y = 10 }, 4)] }])"
        
        let array = ast_may.ToString().Split('\n')
        Assert.AreEqual(expected_firstline, array[0])
        Assert.AreEqual(expected_secondline.Replace(" ",""), array[1].Replace(" ",""))
        Assert.AreEqual(expected_thirdline.Replace(" ",""), array[2].Replace(" ",""))
        Assert.AreEqual(expected_fourthline.Replace(" ",""), array[3].Replace(" ",""))
        Assert.AreEqual(expected_fifthline.Replace(" ",""), array[4].Replace(" ",""))
        Assert.AreEqual(expected_sixthline.Replace(" ",""), array[5].Replace(" ",""))
        Assert.AreEqual(expected_seventhline.Replace(" ",""), array[6].Replace(" ",""))
        Assert.AreEqual(expected_eighthline.Replace(" ",""), array[7].Replace(" ",""))
        Assert.AreEqual(expected_ninthline.Replace(" ",""), array[8].Replace(" ",""))
        Assert.AreEqual(expected_tenthline.Replace(" ",""), array[9].Replace(" ",""))
        Assert.AreEqual(expected_eleventhline.Replace(" ",""), array[10].Replace(" ",""))
        Assert.AreEqual(expected_twelfthline.Replace(" ",""), array[11].Replace(" ",""))
        Assert.AreEqual(expected_thirteenthline.Replace(" ",""), array[12].Replace(" ",""))
        Assert.AreEqual(expected_lastline.Replace(" ","").Replace("\n",""), array.[13].Replace(" ","").Replace("\n",""))

        //TRIED TO JUST Concatenate and COMPARE TWO STRINGS BUT KEPT GETTING spacing ERRORS?:
        // let expected_parsed= 
        //                 """Some(Canvas
        //                 [{ name = "TwoCircles"
        //                 dims = { w = 50
        //                 h = 50 }
        //                 components = [Circle ({ x = 5
        //                 y = 5 }, 5); Circle ({ x = 5
        //                 y = 5 }, 3)] };
        //                 { name = "ThreeCircles"
        //                 dims = { w = 100
        //                 h = 100 }
        //                 components =
        //                 [Name ("TwoCircles", RelativePlacement (Position (Right, 2), 90));
        //                 Circle ({ x = 10
        //                 y = 10 }, 4)] }])"""
        // let nospace_expected = expected_parsed.Replace(" ","").Replace("\n","")
        // //let nospace_expected1 = nospace_expected
        // let nospace_actual = ast_may.ToString().Replace(" ","").Replace("\n","")
        // //let nospace_actual1 = nospace_actual.Replace("\n","")
        // Assert.AreEqual(nospace_expected, nospace_actual)

    [<TestMethod>]
    //Evaluator Test:
    member this.EvalTest () =
        (* try to parse what they gave us *)
        let ast_maybe = parse input do_debug
        let actual_result = 
            match ast_maybe with
            | Some canvas_ast ->
                eval canvas_ast
            | _ -> "Invalid"
                
        let expected_0thline = """<svg width="400" height="400" xmlns="http://www.w3.org/2000/svg" xmlns:xlink="http://www.w3.org/1999/xlink" style="background-color:navajowhite">"""
        let expected_firstline = """   <circle cx ="5" cy ="5" r ="5" stroke= "black" stroke-width="4" fill ="navajowhite"/>"""
        let expected_secondline = """   <circle cx ="5" cy ="5" r ="3" stroke= "black" stroke-width="4" fill ="navajowhite"/>"""
        let expected_thirdline = """   <circle cx ="10" cy ="10" r ="4" stroke= "black" stroke-width="4" fill ="navajowhite"/>"""
        let expected_lastline = """ </svg>"""

        let array = actual_result.ToString().Split('\n')
        Assert.AreEqual(expected_0thline.Replace(" ",""), array[0].Replace(" ",""))
        Assert.AreEqual(expected_firstline.Replace(" ",""), array[1].Replace(" ",""))
        Assert.AreEqual(expected_secondline.Replace(" ",""), array[2].Replace(" ",""))
        Assert.AreEqual(expected_thirdline.Replace(" ",""), array[3].Replace(" ",""))
        Assert.AreEqual(expected_lastline.Replace(" ",""), array[4].Replace(" ",""))

        //TRIED TO JUST Concatenate and COMPARE TWO STRINGS BUT KEPT GETTING spacing ERRORS?:
        //         let expected_output = """svg width="400" height="400" xmlns="http://www.w3.org/2000/svg" xmlns:xlink="http://www.w3.org/1999/xlink" style="background-color:navajowhite">
        //    <circle cx ="5" cy ="5" r ="5" stroke= "black" stroke-width="4" fill ="navajowhite"/>
        //    <circle cx ="5" cy ="5" r ="3" stroke= "black" stroke-width="4" fill ="navajowhite"/>
        //    <circle cx ="10" cy ="10" r ="4" stroke= "black" stroke-width="4" fill ="navajowhite"/>
        // </svg>"""
                // let nospace_expected = expected_output.Replace(" ","").Replace("\n","")

                // let nospace_actual = actual_result.ToString().Replace(" ","").Replace("\n","")
                // //let nospace_actual1 = nospace_actual.Replace("\n","")
                // Assert.AreEqual(nospace_expected, nospace_actual)