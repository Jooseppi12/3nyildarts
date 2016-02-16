namespace _3nyildarts

open WebSharper
open WebSharper.JavaScript
open WebSharper.JQuery
open WebSharper.UI.Next
open WebSharper.UI.Next.Client

[<JavaScript>]
module Client =    
    type IndexTemplate = Templating.Template<"index.html">

    let Map = [(1, ["1"]); (2, ["2"; "D1"]); (3, ["3"]); (4, ["D2"; "4"]);
                (5, ["5"]); (6, ["6"; "D3"; "T2"]); (7, ["7"]); (8, ["D4"; "8"]);
                (9, ["9"]); (10, ["10"; "D5"]); (11, ["11"]); (12, ["D6"; "12"; "T4"]);
                (13, ["13"]); (14, ["14"; "D7"]); (15, ["15";  "T5"]); (16, ["D8"; "16"]);
                (17, ["17"]); (18, ["18"; "D9"]); (19, ["19"]); (20, ["D10"; "20"]);
                (21, ["T7"]); (22, ["D11"]); (24, ["D12"; "T8"]);
                (25, ["25"]); (26, ["D13"]); (27, ["T9"]); (28, ["D14"]);
                (30, ["D15"]); (32, ["D16"]);
                (34, ["D17"]); (36, ["D18"; "T12"]); (38, ["D19"]);
                (40, ["D20"]); (42, ["T14"]); (45, ["T15"]);
                (48, ["T16"]); (50, ["D25"]); (51, ["T17"]); (54, ["T18"]);
                (57, ["T19"]); (60, ["T20"])
                ]|> List.rev

    let rec cartesian lstlst =
            match lstlst with
            | h::[] ->
                List.fold (fun acc elem -> [elem]::acc) [] h
            | h::t ->
                List.fold (fun cacc celem ->
                    (List.fold (fun acc elem -> (elem::celem)::acc) [] h) @ cacc
                    ) [] (cartesian t)
            | _ -> []

    let Res2 = Seq.empty |> ListModel.FromSeq

    let rec seged3 (t:int) (l:list<int*list<string>>) (ls:list<string>) (lt:list<string>) (lu:list<string>) (p:int) =
            if t=0 then
                let E = ls::lt::lu::[]
                List.map (fun x -> String.concat " - " (List.toSeq x)) (cartesian E)
            else
                []

    let rec seged2 (t:int) (l:list<int*list<string>>) (ls:list<string>) (lt:list<string>) (p:int) =
            if t=0 then
                let E = ls::lt::[]
                List.map (fun x -> String.concat " - " (List.toSeq x)) (cartesian E)
            else
                if t<0 then
                    []
                else
                    match l with
                    |[] -> []
                    |hd::tl ->
                        if (fst hd) <= p then
                            let Q = t - fst hd
                            List.append (seged3 Q l ls lt (snd hd) (fst hd)) (seged2 t tl ls lt p)
                        else
                            []

    let rec seged (t:int) (l:list<int*list<string>>) (ls:list<string>) (p:int) =
            if t=0 then
                ls
            else
                if t<0 then
                    []
                else
                    match l with
                    |[] -> []
                    |hd::tl ->
                        if (fst hd) <= p then
                            let Q = t - fst hd
                            List.append (seged2 Q l ls (snd hd) (fst hd)) (seged t tl ls p)
                        else
                            []

    let rec create (t:int) (l:list<int*list<string>>) =
            match l with
            | [] -> []
            | hd::tl ->
                if t<0 then
                    []
                else
                    if t>180 then
                        []
                    else
                        let Q = t - fst hd
                        List.append (seged Q l (snd hd) (fst hd)) (create t tl)

    let Main =
        JQuery.Of("#main").Empty().Ignore

        let newName = Var.Create ""

        IndexTemplate.Main.Doc(
            Name = newName,
            Add = (fun el ev ->
                Res2.Clear ()
                let A = create (int newName.Value) Map
                if A = [] then
                    let Result = ["Nem lehetseges"]
                    Res2.Set(Result|>Seq.ofList)
                    newName.Value <- ""
                else
                    Res2.Set(A|>Seq.ofList)
                    newName.Value <- ""
            ),
            ListContainer = [
                Res2.View.DocSeqCached(fun (name) ->
                    IndexTemplate.ListItem.Doc(Name = View.Const name)
                )
            ]
        )
        |> Doc.RunById "main"
