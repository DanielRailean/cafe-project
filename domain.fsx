type CoffeeType = Arabica | Robusta | Black | Espresso | Cappuccino | Latte | Mocha | Americano | Macchiato | Frappuccino | IcedCoffee
type TeaType = Green | Black | White | Oolong | Herbal
type JuiceType = Orange | Apple | Strawberry | Lemonade | Pineapple
type SodaType = Cola | Fanta | Sprite | Pespi
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


type VIAPerson = { name: string; phoneNumber: string }
type SOSUPerson = { name: string; phoneNumber: string }
type Customer =
    | VIA of VIAPerson
    | SOSU of SOSUPerson
    

type DrinkRecord = { drinkType: DrinkType; size: Size}
type Product = Drink of DrinkRecord


let drink = { drinkType = Coffee Arabica; size = Small }


type Order = { customer: Customer; drinks: drink list; paymentType: PaymentType }

let calculateProductPrice :DrinkType =
    match a with
    | Coffee -> calCCoffee a
    | Tea -> calTea a

let calCCoffee typeOfCoffe =
    match cRec.typeOfCoffe
