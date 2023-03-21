open System

type Size = Small | Medium | Large

type DrinkType =
    | Coffee of string
    | Tea of string
    | Juice of string

type Drink = {
    Name: string
    DrinkType: DrinkType
    Size: Size
    Price: float
}

type CustomerId =
    | VIAId of int
    | SOSUId of string

type Customer = {
    Id: CustomerId
    Name: string
}

type PaymentOption = MobilePay | CreditCard | Cash

type OrderDrinkMsg =
    | OrderDrink of Customer * Drink * int * PaymentOption
    | LeaveAComment of string

let drinkMenu : Drink list = [
    { Name = "Latte"; DrinkType = Coffee "Latte"; Size = Small; Price = 15.0 }
    { Name = "Espresso"; DrinkType = Coffee "Espresso"; Size = Small; Price = 12.0 }
    { Name = "Cappuccino"; DrinkType = Coffee "Cappuccino"; Size = Small; Price = 18.0 }
    { Name = "Green Tea"; DrinkType = Tea "Green"; Size = Small; Price = 10.0 }
    { Name = "Black Tea"; DrinkType = Tea "Black"; Size = Small; Price = 10.0 }
    { Name = "Herbal Tea"; DrinkType = Tea "Herbal"; Size = Small; Price = 12.0 }
    { Name = "Apple Juice"; DrinkType = Juice "Apple"; Size = Small; Price = 8.0 }
    { Name = "Orange Juice"; DrinkType = Juice "Orange"; Size = Small; Price = 8.0 }
    { Name = "Grape Juice"; DrinkType = Juice "Grape"; Size = Small; Price = 10.0 }
]

let getPrice (customer: Customer) (drink: Drink) (quantity: int) : float =
    let basePrice = drink.Price * float quantity
    match customer.Id with
    | SOSUId _ -> basePrice * 0.9
    | _        -> basePrice

// function to add VAT to a given price
let addVAT (price: float) (vatInPercent: int)  : float =
    let vat = 100 / vatInPercent
    price + (price * (float vat))

let gtgAgent : MailboxProcessor<OrderDrinkMsg> = MailboxProcessor.Start(fun inbox ->
    let rec processMessages () =
        async {
            let! msg = inbox.Receive()

            match msg with
            | OrderDrink (customer, drink, quantity, paymentOption) ->
                let price = getPrice customer drink quantity
                let priceWithVAT =
                    match drink.DrinkType with
                    | Coffee _ -> addVAT price 25
                    | _        -> price
                printfn "Hello %s\nPlease pay %.2f DKK for your %d %s drinks using %A.\nThanks!" customer.Name priceWithVAT quantity drink.Name paymentOption
            | LeaveAComment comment ->
                printfn "Customer feedback: %s" comment
                printfn "Thank you for your feedback!\n"

            return! processMessages ()
        }
    processMessages ()
)

let findDrinkByName (drinkName: string) (drink: Drink) : bool =
    drink.Name = drinkName

let orderDrink (customer: Customer) (drinkName: string) (quantity: int) (paymentOption: PaymentOption) : unit =
    match List.tryFind (findDrinkByName drinkName) drinkMenu with
    | Some drink -> gtgAgent.Post(OrderDrink(customer, drink, quantity, paymentOption))
    | None -> printfn "Drink not found"

let leaveComment (comment: string) : unit =
    gtgAgent.Post(LeaveAComment comment)

let customers : Customer list = [
    { Id = VIAId 1234; Name = "John Doe" }
    { Id = SOSUId "SOSU-5678"; Name = "Jane Doe" }
]

let findCustomerById (id: CustomerId) : Customer option =
    List.tryFind (fun customer -> customer.Id = id) customers

let orderDrinkById (customerId: CustomerId) (drinkName: string) (quantity: int) (paymentOption: PaymentOption) : unit =
    match findCustomerById customerId with
    | Some customer -> orderDrink customer drinkName quantity paymentOption
    | None -> printfn "Customer not found"

// Test
orderDrinkById (VIAId 1234) "Latte" 2 MobilePay
leaveComment "Great coffee! Loved the taste."
orderDrinkById (SOSUId "SOSU-5678") "Herbal Tea" 1 CreditCard
leaveComment "The herbal tea was soothing and refreshing."

System.Threading.Thread.Sleep(1000)
