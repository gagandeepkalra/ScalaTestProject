import java.io.BufferedReader;
import java.io.InputStreamReader;
import java.util.*;

public class UUIDLogSearch {
    public static void main(String[] args) {
        final Scanner sc = new Scanner(new BufferedReader(new InputStreamReader(System.in)));

        Map<UUID, LinkedList<String>> map = new HashMap<>();

        while (true) {
            String fullString = sc.nextLine();
            if (fullString == null || fullString.equals("")) break;

            final String[] uuidString = fullString.split(" ");
            final UUID uuid = UUID.fromString(uuidString[2]);

            LinkedList<String> ls = map.getOrDefault(uuid, new LinkedList<>());
            ls.addLast(fullString);

            map.put(uuid, ls);
        }

        map.keySet().stream().sorted(Comparator.comparing(UUID::toString)).forEach(uuid -> {
            map.get(uuid).forEach(System.out::println);
            System.out.println();
        });

    }
}
