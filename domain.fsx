type CoffeeType = Arabica | Robusta | Espresso | Cappuccino
type TeaType = Green | Black | White | Oolong | Herbal
type JuiceType = Orange | Apple | Strawberry | Lemonade | Pineapple
type SodaType = Cola | Fanta | Sprite
type MilkType = WholeMilk | SemiSkimmedMilk | SkimmedMilk | AlmondMilk | SoyMilk | CoconutMilk

type Size = Small | Medium | Large

type DrinkType = 
    | Coffee of CoffeeType
    | Tea of TeaType
    | Juice of JuiceType
    | Soda of SodaType
    | Milk of MilkType

type CoffeeRecord = { coffeeType: CoffeeType; size: Size }
type TeaRecord = { teaType: TeaType; size: Size; }
type JuiceRecord = { juiceType: JuiceType; size: Size }
type SodaRecord = { sodaType: SodaType; size: Size }
type MilkRecord = { milkType: MilkType; size: Size }


type Cash = { amount: float }
type Card = { cardNumber: string; cvc: string; expiryDate: string }
type MobilePay = { phoneNumber: string }

type PaymentType = Cash | Card | MobilePay


type VIAPerson = { name: string; studentNumber: string }
type SOSUPerson = { name: string; phoneNumber: string }
type Customer =
    | VIA of VIAPerson
    | SOSU of SOSUPerson
    

type DrinkRecord = { drinkType: DrinkType; size: Size}
type Product = Drink of DrinkRecord

type Order = { customer: Customer; drink: DrinkRecord; paymentType: PaymentType }

let drinkOrdered = { drinkType = Coffee Arabica; size = Small }


let getPrice (product: Product) : float =
    let coffeePrices = 
        [ Arabica, 2.0
          Robusta, 1.5
          Espresso, 2.5
          Cappuccino, 3.0 ]
        |> Map.ofList
        
    let teaPrices =
        [ Green, 1.5
          Black, 1.0
          White, 2.0
          Oolong, 2.5
          Herbal, 1.5 ]
        |> Map.ofList
        
    let juicePrices =
        [ Orange, 2.5
          Apple, 2.0
          Strawberry, 3.0
          Lemonade, 2.5
          Pineapple, 3.5 ]
        |> Map.ofList
        
    let sodaPrices =
        [ Cola, 1.0
          Fanta, 1.5
          Sprite, 1.5 ]
        |> Map.ofList
        
    let milkPrices =
        [ WholeMilk, 2.0
          SemiSkimmedMilk, 1.5
          SkimmedMilk, 1.0
          AlmondMilk, 3.0
          SoyMilk, 2.5
          CoconutMilk, 3.5 ]
        |> Map.ofList
    
    let calculatePrice (subType: 'a) (prices: Map<'a, float>) (size: Size) : float =
        match prices.TryFind(subType) with
        | Some price ->
            match size with
            | Small -> price * 0.8
            | Medium -> price
            | Large -> price * 1.2
        | None -> failwith "Invalid subtype"
    
    match product with
    | Drink { drinkType = Coffee coffeeType; size = size } ->
        calculatePrice coffeeType coffeePrices size
    | Drink { drinkType = Tea teaType; size = size } ->
        calculatePrice teaType teaPrices size
    | Drink { drinkType = Juice juiceType; size = size } ->
        calculatePrice juiceType juicePrices size
    | Drink { drinkType = Soda sodaType; size = size } ->
        calculatePrice sodaType sodaPrices size
    | Drink { drinkType = Milk milkType; size = size } ->
        calculatePrice milkType milkPrices size


let myCoffee = { drinkType = Coffee Arabica; size = Medium }
let price = getPrice (Drink myCoffee)
printfn "Price: $%.2f" price