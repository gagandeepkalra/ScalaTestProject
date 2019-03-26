package expedia;

import java.util.*;

class Record {
    String name;
    long number;

    Record(String name, long number) {
        this.name = name;
        this.number = number;
    }
}

class AddressBook {
    List<Record> list;

    //declare 'list' as a linked list in the constructor using Java's built in API's

    AddressBook() {
        list = new LinkedList<>();
    }

    public void add(String name, long number) {
        list.add(new Record(name, number));
        System.out.println("Successfully added: " + name);
        //Wrap all the details into an object of class Record and add it to the linked list
        // Print: 'Successfully added: contact_name', where contact_name is the name of the contact added
    }

    public void findByNumber(long number) {
        Iterator<Record> itr = list.iterator();

        boolean found = false;

        while (itr.hasNext()) {
            Record record = itr.next();
            if (record.number == number) {
                found = true;
                System.out.println("Name: " + record.name);
                break;
            }
        }

        if (!found) {
            System.out.println("No such Number exists");
        }
        //Check if the number exists
        // If it doesn't, print: 'No such Number exists'
        //else Print: 'Name: contact_name', where contact_name is the name of the contact having that number
    }

    public void delete(long number) {

        boolean removed = false;

        for (int i = 0; i < list.size(); i++) {
            if (list.get(i).number == number) {
                list.remove(i);
                removed = true;
                System.out.println("Successfully deleted: " + number);
                break;
            }
        }

        if (!removed) {
            System.out.println("No such Number exists");
        }
        //Check if the number exists
        // If it doesn't, print: 'No such Number exists'
        //else delete the item in the linked list having the given number
        // Print: 'Successfully deleted: contact_number', where contact_number is the number to be deleted
    }

    public void printAddressBook() {
        System.out.println("List of contacts:");
        // Print the details of all the contacts in the list in the following format:
        //Name: ABC Number: 123456789
        //Note that the above is just an example

        list.forEach(record -> System.out.println("Name: " + record.name + " Number: " + record.number));
    }
}

public class Source {
    public static void main(String[] args) {
        AddressBook myContacts = new AddressBook();
        myContacts.add("John", 9876123450l);
        myContacts.add("Mellisa", 8360789114l);
        myContacts.add("Daman", 9494149900l);
        myContacts.findByNumber(9998760333l);
        myContacts.printAddressBook();
        myContacts.delete(9876123450l);
        myContacts.add("Gregory", 7289880988l);
        myContacts.printAddressBook();
        myContacts.findByNumber(8360789114l);
        myContacts.add("Mary", 7205678901l);
        myContacts.delete(9876123450l);
        myContacts.findByNumber(7205678901l);
        myContacts.printAddressBook();
    }
}