class HighConditional6$Account {

    double balance;

    HighConditional6$ErrorLog errorLog = new HighConditional6$ErrorLog();

    HighConditional6$TransactionLog transactionLog = new HighConditional6$TransactionLog();

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

class HighConditional6$AccountOwner {
    private HighConditional6$Account account;

    public HighConditional6$AccountOwner(HighConditional6$Account account) {
        this.account = account;
    }

    public void payBeneficiary(HighConditional6$Beneficiary b, double amount) {
        boolean transactionPossible = this.account.withdraw(amount);
        if (transactionPossible) {
            b.receive(amount);
        }
    }

}

class HighConditional6$Beneficiary {

    private double received;

    public void receive(double amount) {
        this.received += amount;
    }
}

class HighConditional6$ErrorLog {

    public void logError(String message) {
        System.out.println(message);
    }
}

class HighConditional6$Main {
    public static void main(String[] args) {
        HighConditional6$Account account = new HighConditional6$Account();
        account.deposit(100);
        HighConditional6$AccountOwner owner = new HighConditional6$AccountOwner(account);
        HighConditional6$Beneficiary beneficiary = new HighConditional6$Beneficiary();
        owner.payBeneficiary(beneficiary, 150);
    }
}

class HighConditional6$TransactionLog {

    public void logTransaction(String message) {
    }

}
