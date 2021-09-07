# Budgetable

> A tool for visualizing and managing budgets

----------------------

## How to Use

Budgetable tries to help people visualize and forecast their budgets by applying recurring transactions to records of their accounts.

---------------------

### Accounts

In order to track accounts, you have to create them. The _"Add new account"_ button lets you edit the details for a newly tracked account.

1. You must give it a _unique_ name
2. You can designate a _restriction_ on the account (credit cards are only negative, share certificates are only positive, checking accounts have no restrictions, etc.)
3. Give it a color if you want (using [html color names](https://htmlcolorcodes.com/color-names/), or [html hex color codes](https://htmlcolorcodes.com/))
4. Its initial balance

You **must** save the accounts being edited in order to see them in the graph.

-----------------

### Finance Plans

In order to schedule interaction with accounts, you must use a finance plan. Finance plans are one of the following:

- _Income_ (where you don't have access to the account that gave you money)
- _Cost_ (where you don't have access to the account that took your money)
- _Transfer_ (where you have access to both the sending and receiving accounts)

Use the _"Add new finance plan"_ button to add a new plan, and select its type (transfer, income, or cost).

#### Repeating Finance Plans

Finance plans can repeat - either daily, weekly, monthly, or yearly.

> **NOTE**: Work is being done to add more functionality to repeating finance plan schedules - limiting repeating transfer between dates, and perform them by some interval (every 3rd day, every other month, etc.).

#### Transfer

1. Select which account funds are getting taken from
2. Select which account funds are being sent to
3. Is it a repeating transfer?
4. The value of the transfer
5. An optional note for personal organization

#### Income

2. Select which account funds are being sent to
3. Is it repeating income?
4. The value of the income
5. An optional note for personal organization

#### Cost

2. Select which account funds are being taken from
3. Is it a repeating cost?
4. The value of the cost
5. An optional note for personal organization

----------------------

### Graph Calculation

You can designate the start date for which your forecast will begin (defaults to today), and likewise the interval being graphed. For instance, if you'd like to view your forecasts over the next 40 weeks, simply set the interval to _"Weekly"_ and the value to `40`.

----------------------

## Building the Software

Please see the [BUILDING.md](./BUILDING.md) file for details.

----------------------

## Contributing

Contributions are very welcome! Please feel free to [file issues](https://github.com/budgetable/budgetable.github.io/issues), or submit merge requests!

----------------------

## License

[LICENSE](./LICENSE) summarized:

Budgetable, budget visualization software

Copyright (C) 2021  Athan Lawrence Clark

This program is free software: you can redistribute it and/or modify
it under the terms of the GNU General Public License as published by
the Free Software Foundation, [version 3 of the License](https://www.gnu.org/licenses/gpl-3.0.en.html).

This program is distributed in the hope that it will be useful,
but WITHOUT ANY WARRANTY; without even the implied warranty of
MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
GNU General Public License for more details.

You should have received a copy of the GNU General Public License
along with this program.  If not, see https://www.gnu.org/licenses/.
