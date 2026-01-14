# SimulaCredito - Loan Simulation App Product Definition

## Product Vision
A consumer-facing React Native app that helps users understand, compare, and optimize their loans and debts. Built on the hasban calculation engine, targeting Brazil first with international expansion planned.

---

## Core Use Cases

### 1. Refinancing Discovery
**User Story**: "I'm paying R$500/month but don't know if I'm being ripped off"
- Input current loan terms (payment, remaining months, original amount)
- Reveal "true cost" with hidden fees/interest breakdown (shock value moment)
- Compare with fair market rates based on user profile
- Show potential savings from refinancing

### 2. Debt Escape Planning
**User Story**: "I have R$8,000 in credit card debt and feel trapped"
- Input credit card balances and rates
- Show payoff timeline at minimum payments (shock: "94 months!")
- Present strategies: Snowball, Avalanche, Consolidation
- Visualize path to debt freedom with milestones

### 3. Asset Purchase Planning
**User Story**: "I want to buy a R$30,000 car, what will it really cost me?"
- Input purchase price and down payment
- Estimate rate range based on user profile (income, debt ratio)
- Compare term lengths (12/24/36/48 months)
- Show total cost vs. cash price premium

---

## User Profile (Progressive Disclosure)

### Level 1 - Basic (Required at onboarding)
- Employment status: employed | self-employed | student | retired | other
- Monthly income (slider: R$0 - R$50,000+)
- Has existing debts: yes | no

### Level 2 - Financial (Unlocks better rate estimates)
- Net income after deductions
- Monthly fixed expenses
- Current debt payments
- Savings rate

### Level 3 - Risk Profile (Computed + optional)
- Debt-to-income ratio (computed)
- Estimated credit score: poor | fair | good | excellent
- Risk tolerance preference

---

## Information Architecture

```
App Root
├── Onboarding (first launch)
│   ├── Welcome (3 carousel slides)
│   ├── Quick Profile Setup
│   └── Interactive First Simulation Tutorial
│
└── Main Tabs
    ├── [1] Home Dashboard
    │   ├── Financial Health Score widget
    │   ├── Quick Actions grid
    │   ├── Recent Simulations
    │   └── Tips & Insights
    │
    ├── [2] Simulate
    │   ├── Refinancing Discovery wizard
    │   ├── Debt Escape wizard
    │   ├── Asset Purchase wizard
    │   └── Custom Loan wizard
    │
    ├── [3] My Debts (Tracker)
    │   ├── Active debts list
    │   ├── Payoff progress
    │   └── Milestone celebrations
    │
    ├── [4] Learn
    │   ├── Financial education articles
    │   ├── Glossary
    │   └── Simple calculators
    │
    └── [5] Profile
        ├── Profile management
        ├── Settings
        └── Data export
```

---

## UX Design Principles

### Style: Friendly Illustrated
- Warm color palette: Green (savings/positive), Orange (friendly), Red (debt/warning)
- Custom illustrations for empty states, achievements, milestones
- Gamification elements throughout

### Onboarding: Interactive Tutorial
- Guided first simulation with contextual tooltips
- Progressive profile collection - start minimal, unlock features with more data

### Results: Summary First
- Key numbers prominent (monthly payment, total cost, savings)
- Expandable sections for detailed breakdowns
- "Shock value" animated counters for impact

---

## Hasban API Integration

### Endpoints (existing)
```
POST /hasban/personal   - Personal loan calculation
POST /hasban/creditcard - Credit card analysis
POST /hasban/net        - Find principal for desired net
```

### Key Request/Response Types

**Personal Loan Request**:
```json
{
  "principal": 10000,
  "annual_rate": 0.12,
  "terms": 24,
  "start_year": 2024, "start_month": 1, "start_day": 15,
  "amortization": "price",
  "frequency": "monthly",
  "fees": [{"type": "percentage", "value": 0.02}],
  "grace_period": {"terms": 3, "capitalize": true}
}
```

**Credit Card Request**:
```json
{
  "balance": 5000,
  "annual_rate": 0.15,
  "min_type": "greater_of",
  "min_fixed": 50,
  "min_percent": 0.02
}
```

### Features Supported by Hasban
- Amortization types: PRICE (constant payment), SAC (decreasing), Bullet
- Payment frequencies: Monthly, Biweekly, Weekly, Daily, Yearly
- Fees: Fixed and percentage-based
- Grace periods with optional capitalization
- Brazilian IOF tax calculations
- CETIP21 precision compliance (252 business days)

---

## Data Models

### User Profile
```typescript
interface UserProfile {
  id: string;
  basic: { employmentStatus, monthlyIncome, hasExistingDebts };
  financial?: { netIncome, monthlyExpenses, existingDebtPayments };
  riskProfile?: { debtToIncomeRatio, estimatedCreditScore };
  preferences: { currency, locale, notifications };
  achievements: Achievement[];
}
```

### Simulation Types
```typescript
interface LoanSimulation {
  type: 'personal_loan';
  input: { principal, annualRate, terms, amortization, fees, gracePeriod };
  result: { monthlyPayment, totalInterest, totalFees, schedule[] };
}

interface RefinancingSimulation {
  type: 'refinancing';
  currentLoan: { monthlyPayment, remainingMonths, estimatedRate };
  proposedLoans: LoanSimulation[];
  comparison: { currentTotalCost, bestAlternativeCost, potentialSavings };
}

interface DebtEscapePlan {
  type: 'debt_escape';
  debts: CreditCardSimulation[];
  strategies: { snowball, avalanche, consolidation };
  monthlyBudget: number;
}
```

---

## Local Storage Schema

```typescript
const STORAGE_KEYS = {
  USER_PROFILE: '@simula/user/profile',
  SIMULATION_INDEX: '@simula/simulations/index',
  SIMULATION_PREFIX: '@simula/simulations/item/',
  DEBTS_INDEX: '@simula/debts/index',
  ACHIEVEMENTS: '@simula/achievements',
  ONBOARDING_COMPLETE: '@simula/app/onboarding',
};
```

Estimated max storage: ~8MB (100 simulations, 50 tracked debts)

---

## Gamification Features

### Achievements
| Achievement | Trigger | XP |
|-------------|---------|-----|
| First Steps | Complete onboarding | 10 |
| Reality Check | View shock value on any debt | 15 |
| Savings Hunter | Find R$500+ in savings | 30 |
| Milestone Master | Hit 25% payoff | 40 |
| Debt Free | Complete any payoff | 200 |

### Progress Visualization
- Payoff progress bars with Bronze/Silver/Gold/Platinum milestones
- Celebration animations at 25%/50%/75%/100% payoff
- Weekly progress summaries

---

## Technical Architecture

```
┌──────────────────┐     ┌──────────────────┐     ┌──────────────────┐
│   React Native   │────▶│   API Gateway    │────▶│  Hasban Backend  │
│   (Expo)         │     │   (REST/JSON)    │     │  (Haskell/Warp)  │
├──────────────────┤     └──────────────────┘     └──────────────────┘
│ - Redux Toolkit  │
│ - React Query    │
│ - AsyncStorage   │
│ - React Nav 6    │
│ - i18n (pt-BR)   │
└──────────────────┘
```

### React Native Project Structure
```
src/
├── api/           # hasbanClient.ts, hooks (usePersonalLoan, useCreditCard)
├── components/    # core/ (design system), screens/ (per-screen)
├── screens/       # Onboarding, Dashboard, Simulate/*, DebtTracker, Profile
├── store/         # Redux slices (user, simulation, debt, ui)
├── hooks/         # useStorage, useAchievements, useOfflineCalc
├── utils/         # formatters, calculations, analytics
├── i18n/          # pt-BR.json, en-US.json
└── theme/         # colors, typography, spacing
```

---

## Brazil-Specific Features

### IOF Tax Integration
- Automatic IOF calculation: 0.38% base + 0.0082%/day (capped at 365 days)
- Display IOF breakdown in cost analysis
- Show CET (Custo Efetivo Total) including all fees

### CETIP21 Compliance
- 252 business day calculations
- Precision: 9 decimals for factors, 2 for financial amounts
- Toggle for different interest calculation modes

---

## Future Roadmap

### Phase 1: MVP (Brazil Launch)
- Core 3 use cases (Refinancing, Debt Escape, Purchase Planning)
- Portuguese (Brazil) only
- Local storage, anonymous users
- Basic gamification

### Phase 2: Monetization
- Ad placements (native banners, interstitials)
- Premium subscription (unlimited simulations, no ads, export formats)
- Referral program

### Phase 3: Account System
- User registration/login
- Cloud sync of simulations
- Push notifications for payment reminders
- Multi-device support

### Phase 4: International
- Spanish, English languages
- MXN, USD, EUR currencies
- Country-specific tax configurations

---

## Critical Files Reference

| Hasban File | Purpose |
|-------------|---------|
| `src/LoanApi.hs` | REST API endpoints and handlers |
| `src/Hasban/Types.hs` | Core data types (Money, Rate, PersonalLoan, etc.) |
| `src/Hasban/Loan.hs` | Payment and rate calculations |
| `src/Hasban/Amortization.hs` | Schedule generation (PRICE/SAC/Bullet) |
| `src/Hasban/CreditCard.hs` | Credit card analysis and payoff simulation |
| `src/Hasban/Extension/Brazil/Tax.hs` | IOF tax calculations |
| `src/Hasban/Extension/Brazil/CETIP21.hs` | Brazilian market precision |

---

## Verification

### Testing the Backend
```bash
# Start hasban API server
stack run

# Test personal loan endpoint
curl -X POST http://localhost:8081/hasban/personal \
  -H "Content-Type: application/json" \
  -d '{"principal":10000,"annual_rate":0.12,"terms":24,"start_year":2024,"start_month":1,"start_day":15,"amortization":"price","frequency":"monthly"}'

# Test credit card endpoint
curl -X POST http://localhost:8081/hasban/creditcard \
  -H "Content-Type: application/json" \
  -d '{"balance":5000,"annual_rate":0.15,"min_type":"percent","min_percent":0.02}'
```

### React Native App Testing
1. Set up Expo project with TypeScript
2. Implement API client with React Query
3. Create mock data for offline development
4. Test each wizard flow end-to-end
5. Verify calculations match hasban outputs
