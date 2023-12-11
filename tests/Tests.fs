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
        let expected_lastline = "                 y = 10 }, 4)] }])"
        
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
        let array = ast_may.ToString().Split('\n')
        let leng = (array.Length)
        Assert.AreEqual(expected_firstline, array[0])
        //Assert.AreEqual(expected_secondline, array[1])
        Assert.AreEqual(expected_lastline, array.[leng-1])
        
        //Assert.AreEqual(expected_lastline, ast_may.ToString().Split('\n').[-1])

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
                
        //let image =   load path
        let expected_firstline = """   <circle cx ="5" cy ="5" r ="5" stroke= "black" stroke-width="4" fill ="navajowhite"/>"""
        let expected_secondline = """   <circle cx ="5" cy ="5" r ="3" stroke= "black" stroke-width="4" fill ="navajowhite"/>"""
        let expected_thirdline = """   <circle cx ="10" cy ="10" r ="4" stroke= "black" stroke-width="4" fill ="navajowhite"/>"""
//         let expected_output = """svg width="400" height="400" xmlns="http://www.w3.org/2000/svg" xmlns:xlink="http://www.w3.org/1999/xlink" style="background-color:navajowhite">
//    <circle cx ="5" cy ="5" r ="5" stroke= "black" stroke-width="4" fill ="navajowhite"/>
//    <circle cx ="5" cy ="5" r ="3" stroke= "black" stroke-width="4" fill ="navajowhite"/>
//    <circle cx ="10" cy ="10" r ="4" stroke= "black" stroke-width="4" fill ="navajowhite"/>
// </svg>"""
        let array = actual_result.ToString().Split('\n')
        // check height
        Assert.AreEqual(expected_firstline, array[1])
        Assert.AreEqual(expected_secondline, array[2])
        Assert.AreEqual(expected_thirdline, array[3])
        //Assert.AreEqual(expected_output, actual_result)