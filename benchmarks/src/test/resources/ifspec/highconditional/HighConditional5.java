class HighConditional5$Account {

    double balance;

    HighConditional5$ErrorLog errorLog = new HighConditional5$ErrorLog();

    HighConditional5$TransactionLog transactionLog = new HighConditional5$TransactionLog();

    public void deposit(double amount) {
        if (amount > 0) {
            this.balance += amount;
            this.logTransaction(true);
        } else {
            this.logError("Cannot deposit a non-positive amount.");
        }
    }

    public boolean withdraw(double amount) {
        if (amount > 0) {
            double newAmount = this.balance - amount;
            if (newAmount > 0) {
                this.balance = newAmount;
                this.logTransaction(false);
                return true;
            } else {
                this.logError("Account has insufficient funds to withdraw " + amount);
                return false;
            }
        }
        this.logError("Cannot withdraw a non-positive amount.");
        return false;
    }

    private void logTransaction(boolean isDeposit) {
        String transaction = isDeposit ? "Deposit" : "Withdrawal";
        this.transactionLog.logTransaction(transaction + " completed, new balance: " + this.balance);
    }

    public void logError(String message) {
        this.errorLog.logError(message);
    }

}

class HighConditional5$AccountOwner {
    private HighConditional5$Account account;

    public HighConditional5$AccountOwner(HighConditional5$Account account) {
        this.account = account;
    }

    public void payBeneficiary(HighConditional5$Beneficiary b, double amount) {
        boolean transactionPossible = this.account.withdraw(amount);
        if (transactionPossible) {
            b.receive(amount);
        }
    }

}

class HighConditional5$Beneficiary {

    private double received;

    public void receive(double amount) {
        this.received += amount;
    }
}

class HighConditional5$ErrorLog {

    public void logError(String message) {
        System.out.println(message);
    }
}

class HighConditional5$Main {
    public static void main(String[] args) {
        HighConditional5$Account account = new HighConditional5$Account();
        account.deposit(100);
        HighConditional5$AccountOwner owner = new HighConditional5$AccountOwner(account);
        HighConditional5$Beneficiary beneficiary = new HighConditional5$Beneficiary();
        owner.payBeneficiary(beneficiary, 50);
    }
}

class HighConditional5$TransactionLog {

    public void logTransaction(String message) {
    }

}