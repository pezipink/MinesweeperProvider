namespace MinesweeperProvider

open System.Reflection
open Microsoft.FSharp.Core.CompilerServices
open Microsoft.FSharp.Quotations
open Samples.FSharp.ProvidedTypes

[<TypeProvider>]
type Provider(config: TypeProviderConfig) as this = 
    inherit TypeProviderForNamespaces()

    let ns = "MinesweeperProvider"
    let asm = Assembly.GetExecutingAssembly()
    let rnd = System.Random(System.DateTime.Now.Millisecond)

    let createTypes width height mineCount rootTypeName =        
      let rootType = ProvidedTypeDefinition(asm,ns,rootTypeName,None,HideObjectMethods=true)
      let startType = ProvidedTypeDefinition("Start",None)
      let field = Array.init height (fun y -> Array.init width (fun x -> '.'))
      let rec addMines count =
        if count < 0 then () else
        let x,y = rnd.Next width, rnd.Next height
        if field.[y].[x] = '.' then
            let y' = field.[y]
            let x' = y'.[x] 
            y'.[x] <- '*'
            addMines (count-1)
        else addMines count
  
      addMines mineCount
      let initMines = Minefield(field)
      let toText (mines:Minefield)  = 
          let sb = System.Text.StringBuilder()
          sb.Append("<summary>") |> ignore
          let display = mines.Display
          display |> Array.iteri (fun y row -> 
            sb.Append "<para>|" |> ignore
            row |> Array.iteri (fun x c -> 
                sb.Append c |> ignore
                if c = ' ' then sb.Append " " |> ignore
                sb.Append "|" |> ignore
                )
            sb.Append "</para>" |> ignore
            )
          sb.Append("</summary>") |> ignore 
          sb.ToString()  

      let newName() = System.Guid.NewGuid().ToString()
      let rec createStage name move (mines':Minefield) =
        // perform move if this isn't the first type
        let mines =
            match move with 
            | Some(x,y) -> 
                let mines = mines'.Clone()
                mines.Reveal(x,y) |> ignore
                mines
            | None -> mines'        
        let ty = ProvidedTypeDefinition(name,None)
        rootType.AddMember ty
        let name = if mines.MineHit then "# Mine Field - GAME OVER!" else "# Mine Field"
        let p = ProvidedProperty(name,typeof<obj>, GetterCode = fun _ -> <@@ obj() @@>)
        p.AddXmlDoc(toText mines)
        ty.AddMember(p)
        if not mines.MineHit then // create a property for each available move
            mines.Display |> Array.iteri (fun y row ->             
                row |> Array.iteri (fun x c -> 
                 if c = '#' then ty.AddMemberDelayed(fun _ -> 
                    let p = ProvidedProperty(sprintf "%i:%i" x y, (createStage (newName()) (Some(x,y)) mines) ,GetterCode = fun args -> <@@ obj() @@>)
                    p.AddXmlDoc(sprintf "Reveal square at X%i Y%i !" x y )
                    p)))
        ty

      let startProp = ProvidedProperty("Start",createStage (newName()) None initMines ,GetterCode = fun _ -> <@@ obj() @@>)
      startProp.AddXmlDoc( """<summary>A minefield has been generated.... start sweepin' !</summary>""" )
      rootType.AddMember startProp
      rootType.AddMember(ProvidedConstructor([], InvokeCode = fun _ -> <@@ obj() @@>))
      rootType

    let paramType = ProvidedTypeDefinition(asm, ns, "MinesweeperProvider", None, HideObjectMethods = true)
    let width = ProvidedStaticParameter("width",typeof<int>)    
    let height = ProvidedStaticParameter("height",typeof<int>)    
    let mines = ProvidedStaticParameter("mines",typeof<int>)    
    do paramType.DefineStaticParameters([width;height;mines], fun typeName args -> 
        createTypes 
            (args.[0]:?>int) 
            (args.[1]:?>int) 
            (args.[2]:?>int)         
            typeName )
    do this.AddNamespace(ns, [paramType])
[<assembly:TypeProviderAssembly>] 
do()