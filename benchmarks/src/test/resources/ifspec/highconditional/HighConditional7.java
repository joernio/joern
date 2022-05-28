import java.io.BufferedReader;
import java.io.InputStreamReader;

class HighConditional7$Main {
    // just here to have an entry point for the program
    public static void main(String[] args) throws Exception {
        String exitKeyword = "exit";
        boolean exit = false;

        HighConditional7$PasswordManager pm = new HighConditional7$PasswordManager();

        System.out.println("To exit, type: " + exitKeyword);

        while (!exit) {
            System.out.println("Enter password:");
            BufferedReader br = new BufferedReader(new InputStreamReader(System.in));
            String input = br.readLine();
            exit |= input.equals(exitKeyword);
            pm.tryLogin(input);

            System.out.println("Run completed, run again");
        }
    }
}

class HighConditional7$PasswordManager {
    private String password = "supersecret";
    private int invalidTries = 0;
    private int maximumTries = 10;
    private boolean loggedIn = false;

    public void tryLogin(String tryedPassword) {
        if (this.invalidTries < this.maximumTries) {
            if (this.password.equals(tryedPassword)) {
                this.loggedIn = true;
                this.invalidTries = 0;
            } else {
                this.loggedIn = false;
                this.invalidTries++;
            }
        } else {
            System.out.println("No more password tries allowed");
        }
    }
}