# Hasban API Documentation

**Base URL:** `http://localhost:8081`
**API Prefix:** `/hasban`
**Framework:** Servant (Haskell)
**Server:** Warp HTTP Server on port 8081

---

## Table of Contents

1. [Overview](#overview)
2. [Response Format](#response-format)
3. [Endpoints](#endpoints)
   - [Personal Loan Calculation](#1-personal-loan-calculation)
   - [Credit Card Analysis](#2-credit-card-analysis)
   - [Net Value Calculation](#3-net-value-calculation)
4. [Common Types](#common-types)
5. [Error Handling](#error-handling)
6. [Test Results](#test-results)

---

## Overview

The Hasban API provides financial calculation endpoints for:
- **Personal Loans**: Calculate amortization schedules with various methods (Price, SAC, Bullet)
- **Credit Cards**: Analyze payoff scenarios based on minimum payments
- **Net Value**: Goal-seek to find required principal for desired net disbursement

All endpoints use **POST** method with **JSON** request/response bodies.

---

## Response Format

All API responses use a consistent wrapper format:

### Success Response
```json
{
  "success": true,
  "data": {
    // Response payload
  }
}
```

### Error Response
```json
{
  "success": false,
  "error": "Error message describing what went wrong"
}
```

---

## Endpoints

### 1. Personal Loan Calculation

**Endpoint:** `POST /hasban/personal`

Calculates loan amortization schedules with support for multiple amortization methods, payment frequencies, fees, and grace periods.

#### Request Schema

```json
{
  "principal": 100000,        // Required: Loan amount (must be positive)
  "annual_rate": 0.12,        // Required: Annual interest rate as decimal (12% = 0.12)
  "terms": 12,                // Required: Number of payment periods
  "start_year": 2024,         // Required: Loan start year
  "start_month": 1,           // Required: Loan start month (1-12)
  "start_day": 15,            // Required: Loan start day
  "amortization": "price",    // Required: "price", "sac", or "bullet"
  "frequency": "monthly",     // Required: "monthly", "weekly", "biweekly", "daily", "yearly"
  "fees": [                   // Optional: Array of fees
    {"type": "fixed", "value": 500},
    {"type": "percentage", "value": 0.02}
  ],
  "grace_period": {           // Optional: Grace period configuration
    "terms": 3,               // Number of interest-only payments
    "capitalize": true        // Whether to capitalize interest
  }
}
```

#### Response Schema

```json
{
  "success": true,
  "data": {
    "monthly_payment": 8856.21,    // Regular payment amount
    "net_disbursement": 98000,     // Principal minus fees
    "total_fees": 2000,            // Total fees charged
    "total_interest": 6274.48,     // Total interest over loan life
    "total_payments": 106274.48,   // Sum of all payments
    "schedule": [                  // Amortization schedule
      {
        "number": 1,               // Payment number
        "interest": 948.88,        // Interest portion
        "principal": 7907.33,      // Principal portion
        "total": 8856.21,          // Total payment
        "balance": 92092.67,       // Remaining balance
        "tax": null                // Optional tax (e.g., IOF)
      }
    ]
  }
}
```

#### Amortization Methods

| Method | Description |
|--------|-------------|
| `price` | French amortization - constant payments, decreasing interest |
| `sac` | Constant Amortization System - constant principal, decreasing payments |
| `bullet` | Interest-only payments with principal due at maturity |

#### Payment Frequencies

| Frequency | Periods/Year |
|-----------|--------------|
| `daily` | 365 |
| `weekly` | 52 |
| `biweekly` | 26 |
| `monthly` | 12 |
| `yearly` | 1 |

---

### 2. Credit Card Analysis

**Endpoint:** `POST /hasban/creditcard`

Analyzes credit card payoff scenarios based on balance, APR, and minimum payment rules.

#### Request Schema

```json
{
  "balance": 5000,            // Required: Current balance
  "annual_rate": 0.22,        // Required: APR as decimal (22% = 0.22)
  "min_type": "greater_of",   // Required: "fixed", "percent", or "greater_of"
  "min_fixed": 50,            // Required for "fixed" or "greater_of"
  "min_percent": 0.02         // Required for "percent" or "greater_of"
}
```

#### Response Schema

```json
{
  "success": true,
  "data": {
    "balance": 5000,            // Current balance
    "min_payment": 100,         // Calculated minimum payment
    "monthly_rate": 0.0183,     // Monthly interest rate
    "payoff_months": 137,       // Months to payoff (null if unpayable)
    "total_interest": 8678.06   // Total interest if paid at minimum
  }
}
```

#### Minimum Payment Types

| Type | Description |
|------|-------------|
| `fixed` | Fixed dollar amount (uses `min_fixed`) |
| `percent` | Percentage of balance (uses `min_percent`) |
| `greater_of` | Maximum of fixed and percentage amounts |

**Note:** If `payoff_months` is `null`, the minimum payment doesn't cover monthly interest - the debt will never be paid off.

---

### 3. Net Value Calculation

**Endpoint:** `POST /hasban/net`

Goal-seek calculation to find the required principal amount that yields a desired net disbursement after fees.

#### Request Schema

```json
{
  "desired_net": 10000,       // Required: Target net disbursement
  "annual_rate": 0.12,        // Required: Annual interest rate
  "terms": 12,                // Required: Number of payment periods
  "fees": [                   // Required: Array of fees (can be empty)
    {"type": "fixed", "value": 200},
    {"type": "percentage", "value": 0.02}
  ]
}
```

#### Response Schema

```json
{
  "success": true,
  "data": {
    "principal": 10408.16,      // Required principal amount
    "total_fees": 408.16,       // Total fees calculated
    "net_disbursement": 10000   // Actual net (matches desired)
  }
}
```

---

## Common Types

### Fee Object

```json
{
  "type": "fixed",       // "fixed" or "percentage"
  "value": 500           // Amount (fixed) or rate (percentage as decimal)
}
```

| Fee Type | Description | Example |
|----------|-------------|---------|
| `fixed` | Fixed dollar amount deducted | `{"type": "fixed", "value": 500}` = $500 |
| `percentage` | Percentage of principal | `{"type": "percentage", "value": 0.02}` = 2% |

### Grace Period Object

```json
{
  "terms": 3,             // Number of interest-only periods
  "capitalize": true      // true = add unpaid interest to principal
}
```

---

## Error Handling

### Validation Errors

When business rules are violated (e.g., negative principal):

```json
{
  "success": false,
  "error": "Validation errors: [InvalidPrincipal \"Principal must be positive\"]"
}
```

### Parse Errors (HTTP 400)

When required fields are missing or malformed:

```
Error in $: key "annual_rate" not found
```

---

## Test Results

### Personal Loan Endpoint Tests

| Test Case | Description | Status | Key Results |
|-----------|-------------|--------|-------------|
| 1 | Basic Price Amortization | SUCCESS | Fixed payments of 8,856.21, total interest 6,274.48 |
| 2 | SAC Amortization | SUCCESS | Constant principal 8,333.33, decreasing payments |
| 3 | Bullet Loan | SUCCESS | Interest-only until final balloon payment 50,398.71 |
| 4 | With Fixed Fees | SUCCESS | 500 fee, net disbursement 99,500 |
| 5 | With Percentage Fees | SUCCESS | 2% = 2,000 fee, net disbursement 98,000 |
| 6 | Grace Period (capitalize) | SUCCESS | Higher post-grace payments due to capitalized interest |
| 7 | Grace Period (no capitalize) | SUCCESS | Interest-only during grace, standard payments after |
| 8 | Weekly Frequency | SUCCESS | 52 weekly payments of 206.34 |
| 9 | Missing Fields | ERROR | Parse error for missing required fields |
| 10 | Invalid Principal | ERROR | Validation error: "Principal must be positive" |

#### Test Case 1: Basic Price Amortization

**Request:**
```json
{
  "principal": 100000,
  "annual_rate": 0.12,
  "terms": 12,
  "start_year": 2024,
  "start_month": 1,
  "start_day": 15,
  "amortization": "price",
  "frequency": "monthly"
}
```

**Response:**
```json
{
  "success": true,
  "data": {
    "monthly_payment": 8856.206738944082,
    "net_disbursement": 100000,
    "total_fees": 0,
    "total_interest": 6274.480867328981,
    "total_payments": 106274.48086732898,
    "schedule": [
      {"number": 1, "interest": 948.88, "principal": 7907.33, "total": 8856.21, "balance": 92092.67, "tax": null},
      {"number": 2, "interest": 873.85, "principal": 7982.36, "total": 8856.21, "balance": 84110.31, "tax": null},
      {"number": 3, "interest": 798.11, "principal": 8058.10, "total": 8856.21, "balance": 76052.21, "tax": null},
      "...",
      {"number": 11, "interest": 165.71, "principal": 8690.50, "total": 8856.21, "balance": 8772.96, "tax": null},
      {"number": 12, "interest": 83.24, "principal": 8772.96, "total": 8856.21, "balance": 0, "tax": null}
    ]
  }
}
```

#### Test Case 2: SAC Amortization

**Request:**
```json
{
  "principal": 100000,
  "annual_rate": 0.12,
  "terms": 12,
  "start_year": 2024,
  "start_month": 1,
  "start_day": 15,
  "amortization": "sac",
  "frequency": "monthly"
}
```

**Response:**
```json
{
  "success": true,
  "data": {
    "monthly_payment": 9282.21262679164,
    "net_disbursement": 100000,
    "total_fees": 0,
    "total_interest": 6167.715407478972,
    "total_payments": 111386.55152149967,
    "schedule": [
      {"number": 1, "interest": 948.88, "principal": 8333.33, "total": 9282.21, "balance": 91666.67, "tax": null},
      {"number": 2, "interest": 869.81, "principal": 8333.33, "total": 9203.14, "balance": 83333.33, "tax": null},
      "...",
      {"number": 12, "interest": 79.07, "principal": 8333.33, "total": 8412.41, "balance": 0, "tax": null}
    ]
  }
}
```

#### Test Case 3: Bullet Loan

**Request:**
```json
{
  "principal": 50000,
  "annual_rate": 0.10,
  "terms": 6,
  "start_year": 2024,
  "start_month": 3,
  "start_day": 1,
  "amortization": "bullet",
  "frequency": "monthly"
}
```

**Response:**
```json
{
  "success": true,
  "data": {
    "monthly_payment": 398.7070214451882,
    "net_disbursement": 50000,
    "total_fees": 0,
    "total_interest": 2392.2421286711324,
    "total_payments": 2392.242128671129,
    "schedule": [
      {"number": 1, "interest": 398.71, "principal": 0, "total": 398.71, "balance": 50000, "tax": null},
      {"number": 2, "interest": 398.71, "principal": 0, "total": 398.71, "balance": 50000, "tax": null},
      "...",
      {"number": 6, "interest": 398.71, "principal": 50000, "total": 50398.71, "balance": 0, "tax": null}
    ]
  }
}
```

#### Test Case 4: With Fixed Fees

**Request:**
```json
{
  "principal": 100000,
  "annual_rate": 0.12,
  "terms": 12,
  "start_year": 2024,
  "start_month": 1,
  "start_day": 15,
  "amortization": "price",
  "frequency": "monthly",
  "fees": [{"type": "fixed", "value": 500}]
}
```

**Response:**
```json
{
  "success": true,
  "data": {
    "monthly_payment": 8856.206738944082,
    "net_disbursement": 99500,
    "total_fees": 500,
    "total_interest": 6274.480867328981,
    "total_payments": 106274.48086732898
  }
}
```

#### Test Case 5: With Percentage Fees

**Request:**
```json
{
  "principal": 100000,
  "annual_rate": 0.12,
  "terms": 12,
  "start_year": 2024,
  "start_month": 1,
  "start_day": 15,
  "amortization": "price",
  "frequency": "monthly",
  "fees": [{"type": "percentage", "value": 0.02}]
}
```

**Response:**
```json
{
  "success": true,
  "data": {
    "monthly_payment": 8856.206738944082,
    "net_disbursement": 98000,
    "total_fees": 2000,
    "total_interest": 6274.480867328981,
    "total_payments": 106274.48086732898
  }
}
```

#### Test Case 6: Grace Period (Capitalize)

**Request:**
```json
{
  "principal": 100000,
  "annual_rate": 0.12,
  "terms": 12,
  "start_year": 2024,
  "start_month": 1,
  "start_day": 15,
  "amortization": "price",
  "frequency": "monthly",
  "grace_period": {"terms": 3, "capitalize": true}
}
```

**Response:**
```json
{
  "success": true,
  "data": {
    "monthly_payment": 8856.206738944082,
    "schedule": [
      {"number": 1, "interest": 948.88, "principal": 0, "total": 948.88, "balance": 100000},
      {"number": 2, "interest": 948.88, "principal": 0, "total": 948.88, "balance": 100000},
      {"number": 3, "interest": 948.88, "principal": 0, "total": 948.88, "balance": 100000},
      {"number": 4, "interest": 976.15, "principal": 11003.40, "total": 11979.55, "balance": 91870.33},
      "..."
    ]
  }
}
```

#### Test Case 7: Grace Period (No Capitalize)

**Request:**
```json
{
  "principal": 100000,
  "annual_rate": 0.12,
  "terms": 12,
  "start_year": 2024,
  "start_month": 1,
  "start_day": 15,
  "amortization": "price",
  "frequency": "monthly",
  "grace_period": {"terms": 3, "capitalize": false}
}
```

**Response:**
```json
{
  "success": true,
  "data": {
    "schedule": [
      {"number": 1, "interest": 948.88, "principal": 0, "total": 948.88, "balance": 100000},
      {"number": 2, "interest": 948.88, "principal": 0, "total": 948.88, "balance": 100000},
      {"number": 3, "interest": 948.88, "principal": 0, "total": 948.88, "balance": 100000},
      {"number": 4, "interest": 948.88, "principal": 10696.02, "total": 11644.90, "balance": 89303.98},
      "..."
    ]
  }
}
```

#### Test Case 8: Weekly Frequency

**Request:**
```json
{
  "principal": 10000,
  "annual_rate": 0.15,
  "terms": 52,
  "start_year": 2024,
  "start_month": 1,
  "start_day": 1,
  "amortization": "price",
  "frequency": "weekly"
}
```

**Response:**
```json
{
  "success": true,
  "data": {
    "monthly_payment": 206.33643808441767,
    "net_disbursement": 10000,
    "total_fees": 0,
    "total_interest": 729.4947803897194,
    "total_payments": 10729.49478038972
  }
}
```

#### Test Case 9: Missing Fields (Validation Error)

**Request:**
```json
{
  "principal": 100000
}
```

**Response:**
```
Error in $: key "annual_rate" not found
```

#### Test Case 10: Invalid Principal (Validation Error)

**Request:**
```json
{
  "principal": -100,
  "annual_rate": 0.12,
  "terms": 12,
  "start_year": 2024,
  "start_month": 1,
  "start_day": 15,
  "amortization": "price",
  "frequency": "monthly"
}
```

**Response:**
```json
{
  "success": false,
  "error": "Validation errors: [InvalidPrincipal \"Principal must be positive\"]"
}
```

---

### Credit Card Endpoint Tests

| Test Case | Description | Status | Key Results |
|-----------|-------------|--------|-------------|
| 1 | Fixed Minimum Payment | SUCCESS | $100 fixed, 137 months, $8,678.06 interest |
| 2 | Percentage Minimum Payment | SUCCESS | 2% = $100, same as fixed |
| 3 | Greater Of | SUCCESS | Uses larger of fixed vs percent |
| 4 | High Balance | SUCCESS | Detects unpayable scenario (null months) |
| 5 | Low Balance | SUCCESS | 24 months, $98.91 interest |
| 6 | Missing Fields | ERROR | Parse error for missing `annual_rate` |
| 7 | Invalid Type | ERROR | "Invalid min payment: type=invalid_type" |

#### Test Case 1: Fixed Minimum Payment

**Request:**
```json
{
  "balance": 5000,
  "annual_rate": 0.22,
  "min_type": "fixed",
  "min_fixed": 100
}
```

**Response:**
```json
{
  "success": true,
  "data": {
    "balance": 5000,
    "min_payment": 100,
    "monthly_rate": 0.018333333333333333,
    "payoff_months": 137,
    "total_interest": 8678.061496556453
  }
}
```

#### Test Case 2: Percentage Minimum Payment

**Request:**
```json
{
  "balance": 5000,
  "annual_rate": 0.22,
  "min_type": "percent",
  "min_percent": 0.02
}
```

**Response:**
```json
{
  "success": true,
  "data": {
    "balance": 5000,
    "min_payment": 100,
    "monthly_rate": 0.018333333333333333,
    "payoff_months": 137,
    "total_interest": 8678.061496556453
  }
}
```

#### Test Case 3: Greater Of (Fixed vs Percent)

**Request:**
```json
{
  "balance": 5000,
  "annual_rate": 0.22,
  "min_type": "greater_of",
  "min_fixed": 50,
  "min_percent": 0.02
}
```

**Response:**
```json
{
  "success": true,
  "data": {
    "balance": 5000,
    "min_payment": 100,
    "monthly_rate": 0.018333333333333333,
    "payoff_months": 137,
    "total_interest": 8678.061496556453
  }
}
```

#### Test Case 4: High Balance (Unpayable Scenario)

**Request:**
```json
{
  "balance": 25000,
  "annual_rate": 0.25,
  "min_type": "percent",
  "min_percent": 0.02
}
```

**Response:**
```json
{
  "success": true,
  "data": {
    "balance": 25000,
    "min_payment": 500,
    "monthly_rate": 0.020833333333333332,
    "payoff_months": null,
    "total_interest": 0
  }
}
```

**Note:** `payoff_months: null` indicates minimum payment ($500) doesn't cover monthly interest ($520.83).

#### Test Case 5: Low Balance

**Request:**
```json
{
  "balance": 500,
  "annual_rate": 0.18,
  "min_type": "fixed",
  "min_fixed": 25
}
```

**Response:**
```json
{
  "success": true,
  "data": {
    "balance": 500,
    "min_payment": 25,
    "monthly_rate": 0.015,
    "payoff_months": 24,
    "total_interest": 98.91338608280407
  }
}
```

#### Test Case 6: Missing Fields (Validation Error)

**Request:**
```json
{
  "balance": 5000
}
```

**Response:**
```
Error in $: key "annual_rate" not found
```

#### Test Case 7: Invalid Minimum Type

**Request:**
```json
{
  "balance": 5000,
  "annual_rate": 0.22,
  "min_type": "invalid_type"
}
```

**Response:**
```json
{
  "success": false,
  "error": "Invalid min payment: type=invalid_type fixed=Nothing pct=Nothing"
}
```

---

### Net Value Endpoint Tests

| Test Case | Description | Status | Key Results |
|-----------|-------------|--------|-------------|
| 1 | Basic Net Value with Fixed Fee | SUCCESS | Principal 10,500 for net 10,000 |
| 2 | Net Value with Percentage Fee | SUCCESS | 3% fee calculated correctly |
| 3 | Net Value with Multiple Fees | SUCCESS | Both fee types combined |
| 4 | Higher Amount | SUCCESS | Scales appropriately |
| 5 | Small Amount with Fixed Fee | SUCCESS | Works with smaller amounts |
| 6 | Missing Fields | ERROR | Parse error for missing fields |
| 7 | Empty Fees | SUCCESS | Returns principal = net |

#### Test Case 1: Basic Net Value with Fixed Fee

**Request:**
```json
{
  "desired_net": 10000,
  "annual_rate": 0.12,
  "terms": 12,
  "fees": [{"type": "fixed", "value": 500}]
}
```

**Response:**
```json
{
  "success": true,
  "data": {
    "principal": 10500,
    "total_fees": 500,
    "net_disbursement": 10000
  }
}
```

#### Test Case 2: Net Value with Percentage Fee

**Request:**
```json
{
  "desired_net": 10000,
  "annual_rate": 0.12,
  "terms": 12,
  "fees": [{"type": "percentage", "value": 0.03}]
}
```

**Response:**
```json
{
  "success": true,
  "data": {
    "principal": 10309.278350515464,
    "total_fees": 309.2783505154639,
    "net_disbursement": 10000
  }
}
```

#### Test Case 3: Net Value with Multiple Fees

**Request:**
```json
{
  "desired_net": 10000,
  "annual_rate": 0.12,
  "terms": 12,
  "fees": [
    {"type": "fixed", "value": 200},
    {"type": "percentage", "value": 0.02}
  ]
}
```

**Response:**
```json
{
  "success": true,
  "data": {
    "principal": 10408.163265306122,
    "total_fees": 408.1632653061224,
    "net_disbursement": 10000
  }
}
```

#### Test Case 4: Higher Amount

**Request:**
```json
{
  "desired_net": 50000,
  "annual_rate": 0.10,
  "terms": 24,
  "fees": [{"type": "percentage", "value": 0.025}]
}
```

**Response:**
```json
{
  "success": true,
  "data": {
    "principal": 51282.05128205128,
    "total_fees": 1282.0512820512822,
    "net_disbursement": 50000
  }
}
```

#### Test Case 5: Small Amount with Fixed Fee

**Request:**
```json
{
  "desired_net": 1000,
  "annual_rate": 0.15,
  "terms": 6,
  "fees": [{"type": "fixed", "value": 50}]
}
```

**Response:**
```json
{
  "success": true,
  "data": {
    "principal": 1050,
    "total_fees": 50,
    "net_disbursement": 1000
  }
}
```

#### Test Case 6: Missing Fields (Validation Error)

**Request:**
```json
{
  "desired_net": 10000
}
```

**Response:**
```
Error in $: key "annual_rate" not found
```

#### Test Case 7: Empty Fees Array

**Request:**
```json
{
  "desired_net": 10000,
  "annual_rate": 0.12,
  "terms": 12,
  "fees": []
}
```

**Response:**
```json
{
  "success": true,
  "data": {
    "principal": 10000,
    "total_fees": 0,
    "net_disbursement": 10000
  }
}
```

---

## Source Files

| File | Description |
|------|-------------|
| `src/LoanApi.hs` | API definitions and handlers |
| `app/Main.hs` | Server setup and configuration |
| `src/Hasban/Types.hs` | Core type definitions |
| `src/Hasban/Loan.hs` | Loan calculation logic |
| `src/Hasban/Amortization.hs` | Schedule generation |
| `src/Hasban/CreditCard.hs` | Credit card analysis |
| `src/Date.hs` | Date handling and frequency definitions |

---

*Documentation generated: 2026-01-13*
*API Version: 2.0 (Improved JSON format)*
