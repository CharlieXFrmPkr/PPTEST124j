let salaries = [75000.0; 48000.0; 120000.0; 190000.0; 300113.0; 92000.0; 36000.0]


let filteredSalaries = 
    salaries
    |> List.filter (fun salary -> salary >= 50000.0 && salary <= 100000.0)

let sumSalariesInRange =
    filteredSalaries
    |> List.fold (fun acc salary -> acc + salary) 0.0

printfn "Filtered salaries between $50,000 and $100,000: %A" filteredSalaries
printfn "Sum of salaries between $50,000 and $100,000: %.2f" sumSalariesInRange


let sequence = seq {75000; 48000; 120000; 190000; 300113; 92000; 36000}
let salariesList = Seq.toList sequence
let highIncomeSalaries = List.filter (fun salary -> salary > 100000) salariesList
let sumOfHighIncomeSalaries = List.fold (+) 0 highIncomeSalaries

printfn "High-income salaries: %A" highIncomeSalaries
printfn "The sum of all high-income salaries is %d" sumOfHighIncomeSalaries


let calculateTax salary =
    match salary with
    | s when s <= 49020.0 -> s * 0.15
    | s when s <= 98040.0 -> s * 0.205
    | s when s <= 151978.0 -> s * 0.26
    | s when s <= 216511.0 -> s * 0.29
    | _ -> salary * 0.33

let salariesAndTaxes = List.map (fun salary -> (salary, calculateTax salary)) salaries

salariesAndTaxes |> List.iter (fun (salary, tax) -> printfn "Salary: %.2f, Tax: %.2f" salary tax)


let updateSalary acc salary =
    if salary < 49020.0 then
        (salary + 20000.0) :: acc
    else
        acc

let updatedSalaries = List.fold updateSalary [] salaries

updatedSalaries |> List.iter (printfn "Updated Salary: %.2f")


let productOfEven (n: int) = 
    let rec helper (current: int) (acc: int) = 
        if current <= 0 then acc 
        else helper (current - 2) (acc * current) 
    helper n 1

let productResult = productOfEven 10
printfn "The product of all even numbers from 10 to 1 is %d" productResult


let sumOfMultiplesOf3 (n: int) =
    let rec helper (current: int) (acc: int) =
        if current <= 0 then acc
        else helper (current - 3) (acc + current)
    helper n 0

let sumResult = sumOfMultiplesOf3 27
printfn "The sum of all multiples of 3 up to 27 is %d" sumResult