import java.util.concurrent.atomic.AtomicLongArray;

class AcmeSafeState implements State {

    private AtomicLongArray value;
    private long[] currentValue;

    AcmeSafeState(int length) {
        value = new AtomicLongArray(length);
    }

    public int size() {
        return value.length();
    }

    public long[] current() {
        currentValue = new long[value.length()];
        for (int i = 0; i < currentValue.length; i++) {
            currentValue[i] = value.get(i);
        }
        return currentValue;
    }

    public void swap(int i, int j) {
        value.getAndDecrement(i);
        value.getAndIncrement(j);
    }
}
