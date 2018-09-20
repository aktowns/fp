open System

type HKT<'F, 'A> private (v : obj) =
    private new(token : 'F, v: obj) =
        if Object.ReferenceEquals(token, Unchecked.defaultof<'F>) then 
            raise <| new System.InvalidOperationException("invalid token")
        HKT<'F, 'A>(v)
    member self.Apply(token : 'F) : obj = 
        if Object.ReferenceEquals(token, Unchecked.defaultof<'F>) then 
            raise <| new System.InvalidOperationException("invalid token")
        v
    static member Create<'F, 'T>(token: 'F, value: obj) = new HKT<'F, 'T>(token, value)
        
[<AbstractClass>]
type Functor<'F>() = 
    abstract Map : ('A -> 'B) -> HKT<'F, 'A> -> HKT<'F, 'B>
    
type Maybe<'A> = 
    Just of 'A | Nothing
    
type Maybe private () = 
    static let token = new Maybe()
    static member Inj (value : Maybe<'T>) : HKT<Maybe, 'T> = HKT<_, _>.Create(token, value)
    static member Prj (hkt : HKT<Maybe, 'T>) : Maybe<'T> = hkt.Apply(token) :?> _
    
type MaybeFunctor() = 
    inherit Functor<Maybe>() with 
    override self.Map f fa = 
        match Maybe.Prj fa with 
        | Just v -> Maybe.Inj (Just (f v))
        | Nothing -> Maybe.Inj Nothing
        
let map<'F, 'A, 'B> (dict : Functor<'F>) (f: 'A -> 'B) (fa: HKT<'F, 'A>): HKT<'F, 'B>= 
    dict.Map f fa

[<EntryPoint>]
let main argv =
    let x = Just 10
    let z = x |> Maybe.Inj |> map (MaybeFunctor()) (x) (fun x -> x * 2)
    let y = 
        x |> Maybe.Inj 
          |> MaybeFunctor().Map (fun x -> x * 2) 
          |> Maybe.Prj
    printfn "Hello World %A %A" x y
    0 
