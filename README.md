# hasban

API for calculating amortization schedules in Brazilian fashion (PRICE and SAC) including finance taxes. Compatible with B3 (Brasil, Bolsa, Balcão) methodologies. [See 'Caderno de fórmulas B3' ](https://www.b3.com.br/data/files/0D/04/4E/D6/3344F6100A7B23F6AC094EA8/Caderno%20de%20Formulas%20-%20CCB_%20CCE%20e%20NCE.pdf)

```
### Calculate amortization schedule
POST http://localhost:8081/loan OR POST http://localhost:8081/loan/liquid
Content-Type: application/json

{
    "interestMode": "TwoFiveTwoMonth",
    "loanAmount": 24395.5149588254,
    "interestRate": 0.0896,
    "loanTerms": 12,
    "paymentEveryX": 1,
    "personType": "Legal",
    "paymentFrequency": "Monthly",    
    "startDate": "2022-10-20",
    "firstIncorporationEvent": "2022-10-25",
    "principalGracePeriodTerms": 0,
    "commissions": [
        {
            "commissionType": "Percentage",
            "commissionValue": 10.81
        },
        {
            "commissionType": "Percentage",
            "commissionValue": 12.51
        }
    ],
    "amortizationType": "PRICE"
} 
```


Result in Console:

```
  #    Date        Interest       Principal      Remaining      Payment        Tax          
  0  2022-10-20  R$       0.00  R$       0.00  R$   24395.51  R$       0.00  R$       0.00
  1  2022-10-25  R$       0.00  R$       0.00  R$   24395.51  R$       0.00  R$       0.00
  2  2022-11-25  R$      21.86  R$    2022.96  R$   22372.55  R$    2044.82  R$      12.75
  3  2022-12-25  R$      20.05  R$    2024.77  R$   20347.78  R$    2044.82  R$      15.33
  4  2023-01-25  R$      18.23  R$    2026.59  R$   18321.19  R$    2044.82  R$      17.92
  5  2023-02-25  R$      16.42  R$    2028.40  R$   16292.79  R$    2044.82  R$      20.27
  6  2023-03-25  R$      14.60  R$    2030.22  R$   14262.57  R$    2044.82  R$      22.86
  7  2023-04-25  R$      12.78  R$    2032.04  R$   12230.53  R$    2044.82  R$      25.38
  8  2023-05-25  R$      10.96  R$    2033.86  R$   10196.67  R$    2044.82  R$      27.99
  9  2023-06-25  R$       9.14  R$    2035.68  R$    8160.99  R$    2044.82  R$      30.52
 10  2023-07-25  R$       7.31  R$    2037.51  R$    6123.48  R$    2044.82  R$      33.14
 11  2023-08-25  R$       5.49  R$    2039.33  R$    4084.15  R$    2044.82  R$      35.76
 12  2023-09-25  R$       3.66  R$    2041.16  R$    2042.99  R$    2044.82  R$      38.30
 13  2023-10-25  R$       1.83  R$    2042.99  R$      -0.00  R$    2044.82  R$      38.34

 
InitialAmount:  R$   24395.51
FinalAmount:    R$      -0.00
Tax:            R$     318.56
Commissions:    R$    5689.03
Net:            R$   18387.92
Interest:       R$     142.31
Total PMT:      R$   24537.83
```

Result in JSON: 

```JSON
{
    "errors": null,
    "response": {
        "resultCommissions": 5689.0340883980825,
        "resultFinalPrincipal": -8.077449820120819e-9,
        "resultInterest": 142.31276339,
        "resultMonthlyEffectiveRate": 4.614135584518442,
        "resultMonthlyNominalRate": 8.96e-2,
        "resultNetValue": 18387.917387434853,
        "resultSchedule": {
            "paymentItems": [
                {
                    "accruedInterest": 0,
                    "dueAmount": 0,
                    "dueDate": "2022-10-20",
                    "financialTax": 0,
                    "paymentAmortization": 0,
                    "paymentNo": 0,
                    "remainingPrincipal": 24395.5149588254
                },
                {
                    "accruedInterest": 0,
                    "dueAmount": 0,
                    "dueDate": "2022-10-25",
                    "financialTax": 0,
                    "paymentAmortization": 0,
                    "paymentNo": 1,
                    "remainingPrincipal": 24395.5149588254
                },
                {
                    "accruedInterest": 21.8583814,
                    "dueAmount": 2044.818976851957,
                    "dueDate": "2022-11-25",
                    "financialTax": 12.74667471194278,
                    "paymentAmortization": 2022.9605954519568,
                    "paymentNo": 2,
                    "remainingPrincipal": 22372.55436337344
                },
                {
                    "accruedInterest": 20.04580871,
                    "dueAmount": 2044.818976851957,
                    "dueDate": "2022-12-25",
                    "financialTax": 15.331582429170897,
                    "paymentAmortization": 2024.7731681419568,
                    "paymentNo": 3,
                    "remainingPrincipal": 20347.781195231488
                },
                {
                    "accruedInterest": 18.23161195,
                    "dueAmount": 2044.818976851957,
                    "dueDate": "2023-01-25",
                    "financialTax": 17.921112067828005,
                    "paymentAmortization": 2026.5873649019568,
                    "paymentNo": 4,
                    "remainingPrincipal": 18321.193830329532
                },
                {
                    "accruedInterest": 16.41578967,
                    "dueAmount": 2044.818976851957,
                    "dueDate": "2023-02-25",
                    "financialTax": 20.26577624313493,
                    "paymentAmortization": 2028.4031871819568,
                    "paymentNo": 5,
                    "remainingPrincipal": 16292.790643147577
                },
                {
                    "accruedInterest": 14.59834042,
                    "dueAmount": 2044.818976851957,
                    "dueDate": "2023-03-25",
                    "financialTax": 22.864344807496696,
                    "paymentAmortization": 2030.2206364319568,
                    "paymentNo": 6,
                    "remainingPrincipal": 14262.57000671562
                },
                {
                    "accruedInterest": 12.77926273,
                    "dueAmount": 2044.818976851957,
                    "dueDate": "2023-04-25",
                    "financialTax": 25.384240108811483,
                    "paymentAmortization": 2032.0397141219569,
                    "paymentNo": 7,
                    "remainingPrincipal": 12230.530292593663
                },
                {
                    "accruedInterest": 10.95855514,
                    "dueAmount": 2044.818976851957,
                    "dueDate": "2023-05-25",
                    "financialTax": 27.992020984021664,
                    "paymentAmortization": 2033.8604217119569,
                    "paymentNo": 8,
                    "remainingPrincipal": 10196.669870881706
                },
                {
                    "accruedInterest": 9.1362162,
                    "dueAmount": 2044.818976851957,
                    "dueDate": "2023-06-25",
                    "financialTax": 30.520991630454787,
                    "paymentAmortization": 2035.6827606519569,
                    "paymentNo": 9,
                    "remainingPrincipal": 8160.987110229749
                },
                {
                    "accruedInterest": 7.31224445,
                    "dueAmount": 2044.818976851957,
                    "dueDate": "2023-07-25",
                    "financialTax": 33.138009495785425,
                    "paymentAmortization": 2037.506732401957,
                    "paymentNo": 10,
                    "remainingPrincipal": 6123.480377827793
                },
                {
                    "accruedInterest": 5.48663842,
                    "dueAmount": 2044.818976851957,
                    "dueDate": "2023-08-25",
                    "financialTax": 35.75969255440437,
                    "paymentAmortization": 2039.332338431957,
                    "paymentNo": 11,
                    "remainingPrincipal": 4084.1480393958363
                },
                {
                    "accruedInterest": 3.65939664,
                    "dueAmount": 2044.818976851957,
                    "dueDate": "2023-09-25",
                    "financialTax": 38.30235952267737,
                    "paymentAmortization": 2041.1595802119568,
                    "paymentNo": 12,
                    "remainingPrincipal": 2042.9884591838795
                },
                {
                    "accruedInterest": 1.83051766,
                    "dueAmount": 2044.818976851957,
                    "dueDate": "2023-10-25",
                    "financialTax": 38.336678436737074,
                    "paymentAmortization": 2042.988459191957,
                    "paymentNo": 13,
                    "remainingPrincipal": -8.077449820120819e-9
                }
            ]
        },
        "resultStartPrincipal": 24395.5149588254,
        "resultTax": 318.56348299246554,
        "resultTotalPayment": 24537.82772222349,
        "resultYearlyEffectiveRate": 71.82424746024259,
        "resultYearlyNominalRate": 1.0805144426581315
    }
}
```

Net Loan Result

```JSON
{
    "queryAmount": 32365.881223023705,
    "result": {
        "errors": null,
        "response": {
            "resultCommissions": 7547.723501209128,
            "resultFinalPrincipal": 1.412126948707737e-8,
            "resultInterest": 188.80839387,
            "resultMonthlyEffectiveRate": 4.614135584518442,
            "resultMonthlyNominalRate": 8.96e-2,
            "resultNetValue": 24395.514958588596,
            "resultSchedule": {
                "paymentItems": [
                    {
                        "accruedInterest": 0,
                        "dueAmount": 0,
                        "dueDate": "2022-10-20",
                        "financialTax": 0,
                        "paymentAmortization": 0,
                        "paymentNo": 0,
                        "remainingPrincipal": 32365.881223023705
                    },
                    {
                        "accruedInterest": 0,
                        "dueAmount": 0,
                        "dueDate": "2022-10-25",
                        "financialTax": 0,
                        "paymentAmortization": 0,
                        "paymentNo": 1,
                        "remainingPrincipal": 32365.881223023705
                    },
                    {
                        "accruedInterest": 28.99982958,
                        "dueAmount": 2712.8908014066315,
                        "dueDate": "2022-11-25",
                        "financialTax": 16.911197013479605,
                        "paymentAmortization": 2683.8909718266314,
                        "paymentNo": 2,
                        "remainingPrincipal": 29681.990251197072
                    },
                    {
                        "accruedInterest": 26.59506327,
                        "dueAmount": 2712.8908014066315,
                        "dueDate": "2022-12-25",
                        "financialTax": 20.340631329170574,
                        "paymentAmortization": 2686.2957381366314,
                        "paymentNo": 3,
                        "remainingPrincipal": 26995.69451306044
                    },
                    {
                        "accruedInterest": 24.18814228,
                        "dueAmount": 2712.8908014066315,
                        "dueDate": "2023-01-25",
                        "financialTax": 23.7761976146568,
                        "paymentAmortization": 2688.7026591266313,
                        "paymentNo": 4,
                        "remainingPrincipal": 24306.991853933807
                    },
                    {
                        "accruedInterest": 21.7790647,
                        "dueAmount": 2712.8908014066315,
                        "dueDate": "2023-02-25",
                        "financialTax": 26.886897361435956,
                        "paymentAmortization": 2691.1117367066313,
                        "paymentNo": 5,
                        "remainingPrincipal": 21615.880117227174
                    },
                    {
                        "accruedInterest": 19.36782859,
                        "dueAmount": 2712.8908014066315,
                        "dueDate": "2023-03-25",
                        "financialTax": 30.334455719860905,
                        "paymentAmortization": 2693.5229728166314,
                        "paymentNo": 6,
                        "remainingPrincipal": 18922.35714441054
                    },
                    {
                        "accruedInterest": 16.954432,
                        "dueAmount": 2712.8908014066315,
                        "dueDate": "2023-04-25",
                        "financialTax": 33.677637126627644,
                        "paymentAmortization": 2695.9363694066315,
                        "paymentNo": 7,
                        "remainingPrincipal": 16226.420775003908
                    },
                    {
                        "accruedInterest": 14.53887301,
                        "dueAmount": 2712.8908014066315,
                        "dueDate": "2023-05-25",
                        "financialTax": 37.137417590522844,
                        "paymentAmortization": 2698.3519283966316,
                        "paymentNo": 8,
                        "remainingPrincipal": 13528.068846607277
                    },
                    {
                        "accruedInterest": 12.12114969,
                        "dueAmount": 2712.8908014066315,
                        "dueDate": "2023-06-25",
                        "financialTax": 40.492639388187456,
                        "paymentAmortization": 2700.7696517166314,
                        "paymentNo": 9,
                        "remainingPrincipal": 10827.299194890646
                    },
                    {
                        "accruedInterest": 9.70126008,
                        "dueAmount": 2712.8908014066315,
                        "dueDate": "2023-07-25",
                        "financialTax": 43.964674700136335,
                        "paymentAmortization": 2703.1895413266316,
                        "paymentNo": 10,
                        "remainingPrincipal": 8124.109653564015
                    },
                    {
                        "accruedInterest": 7.27920225,
                        "dueAmount": 2712.8908014066315,
                        "dueDate": "2023-08-25",
                        "financialTax": 47.44289939121154,
                        "paymentAmortization": 2705.6115991566317,
                        "paymentNo": 11,
                        "remainingPrincipal": 5418.498054407384
                    },
                    {
                        "accruedInterest": 4.85497426,
                        "dueAmount": 2712.8908014066315,
                        "dueDate": "2023-09-25",
                        "financialTax": 50.816292296406544,
                        "paymentAmortization": 2708.0358271466316,
                        "paymentNo": 12,
                        "remainingPrincipal": 2710.4622272607526
                    },
                    {
                        "accruedInterest": 2.42857416,
                        "dueAmount": 2712.8908014066315,
                        "dueDate": "2023-10-25",
                        "financialTax": 50.86182369428304,
                        "paymentAmortization": 2710.4622272466313,
                        "paymentNo": 13,
                        "remainingPrincipal": 1.412126948707737e-8
                    }
                ]
            },
            "resultStartPrincipal": 32365.881223023705,
            "resultTax": 422.64276322597914,
            "resultTotalPayment": 32554.689616879583,
            "resultYearlyEffectiveRate": 71.82424746024259,
            "resultYearlyNominalRate": 1.0805144426581315
        }
    }
}
```