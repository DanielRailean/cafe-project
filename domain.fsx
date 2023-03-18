// size types
type Size = 
    | Small 
    | Medium 
    | Large

// drink types
type CoffeeType = Arabica | Robusta | Espresso | Cappuccino
type TeaType = Green | Black | White | Oolong | Herbal
type JuiceType = Orange | Apple | Strawberry | Lemonade | Pineapple
type SodaType = Cola | Fanta | Sprite
type MilkType = WholeMilk | SemiSkimmedMilk | SkimmedMilk | AlmondMilk | SoyMilk | CoconutMilk
type DrinkType = 
    | Coffee of CoffeeType
    | Tea of TeaType
    | Juice of JuiceType
    | Soda of SodaType
    | Milk of MilkType

// record types for each drink type
type CoffeeRecord = 
    { Type : CoffeeType; Size : Size }
type TeaRecord = 
    { Type : TeaType; Size : Size }
type JuiceRecord = 
    { Type : JuiceType; Size : Size }
type SodaRecord = 
    { Type : SodaType; Size : Size }
type MilkRecord = 
    { Type : MilkType; Size : Size }





// payment types
type CashRecord = unit
type CardRecord = { cardNumber: string; cvc: string; expiryDate: string }
type MobilePayRecord = { phoneNumber: string }
type PaymentType = 
    | Cash of CashRecord
    | Card of CardRecord
    | MobilePay of MobilePayRecord



// customer types
type VIAPerson = { name: string; studentNumber: string }
type SOSUPerson = { name: string; phoneNumber: string }
type Customer =
    | VIA of VIAPerson
    | SOSU of SOSUPerson



// product types
type DrinkRecord = { DrinkType: DrinkType; Size: Size}
type Product = 
    | Drink of DrinkRecord

// record type for a single drink order and its quantity
type DrinkOrder = { Drink : DrinkRecord; Quantity : int }


// order type
type Order = { Customer: Customer; Drink: DrinkRecord; Quantity: int; Payment: PaymentType }



// function to calculate unit price of product based on Product type and size
let getUnitPrice (product: Product) : float =
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
    | Drink { DrinkType = Coffee coffeeType; Size = size } ->
        calculatePrice coffeeType coffeePrices size
    | Drink { DrinkType = Tea teaType; Size = size } ->
        calculatePrice teaType teaPrices size
    | Drink { DrinkType = Juice juiceType; Size = size } ->
        calculatePrice juiceType juicePrices size
    | Drink { DrinkType = Soda sodaType; Size = size } ->
        calculatePrice sodaType sodaPrices size
    | Drink { DrinkType = Milk milkType; Size = size } ->
        calculatePrice milkType milkPrices size


// function to calculate the price of an entire drink order
let getOrderPrice (order: DrinkOrder) : float =
    let unitPrice = getUnitPrice (Drink order.Drink)
    let totalQuantity = float order.Quantity
    let totalPrice = unitPrice * totalQuantity
    totalPrice

// function to add VAT to a given price
let addVAT (price: float) : float =
    let vat = 0.25
    price + (price * vat)


// order drink
type OrderDrinkMsg = 
    | OrderDrink of DrinkType * Size * int
    | LeaveAComment of string

// order agent
// Define a mailbox processor to handle messages for ordering drinks and leaving comments
let orderAgent =
    MailboxProcessor.Start(fun inbox ->
        let rec loop () =
            async {
                let! msg = inbox.Receive()
                match msg with
                | OrderDrink(drinkType: DrinkType, size: Size, quantity) ->
                    printfn "Ordering %d of %A (%A)" quantity drinkType size
                    return! loop()
                | LeaveAComment(comment) ->
                    printfn "Leaving a comment: %s" comment
                    return! loop()
            }
        loop()
    )


// send a message to the orderAgent to order a drink
orderAgent.Post(OrderDrink(Coffee Arabica, Small, 2));;
orderAgent.Post(OrderDrink(Tea Green, Large, 1));;
orderAgent.Post(OrderDrink(Juice Orange, Medium, 3));;

// send a message to the orderAgent to leave a comment
orderAgent.Post(LeaveAComment "This is a comment")

