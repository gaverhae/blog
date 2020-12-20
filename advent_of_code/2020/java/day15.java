public class Main {
    static private final int stop = 30_000_000;
    public static void main(String args[]) {
        long start = System.currentTimeMillis();
        int input[] = { 20, 9, 11, 0, 1, 2 };
        int prevs[] = new int[stop];
        for (int i = 0; i < input.length; i++) {
            prevs[input[i]] = i + 1;
        }
        int cur = 2;
        for (int i = input.length; i < stop; i++) {
            int last_said = prevs[cur];
            prevs[cur] = i;
            if (last_said == 0) {
                cur = 0;
            } else {
                cur = i - last_said;
            }
        }
        System.out.println(cur);
        System.out.println(System.currentTimeMillis() - start);
    }
}
