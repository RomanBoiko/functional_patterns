import java.util.*;

public class CommandQueue {

	interface Command {
		void execute();
	}

	static class command1 implements Command {
		public void execute() {
			System.out.println("command1");
		}
	}
	static class command2 implements Command {
		public void execute() {
			System.out.println("command2");
		}
	}
	static class command3 implements Command {
		public void execute() {
			System.out.println("command3");
		}
	}

	public static List produceRequests() {
		List queue = new ArrayList();
		queue.add(new command1());
		queue.add(new command2());
		queue.add(new command3());
		return queue;
	}

	public static void workOffRequests(List queue) {
		for (Iterator it = queue.iterator(); it.hasNext();)
			((Command) it.next()).execute();
	}

	public static void main(String[] args) {
		List queue = produceRequests();
		workOffRequests(queue);
	}
}
