package System.Convert;

import com.healthmarketscience.jackcess.Table;
import java.util.Map;
import java.util.Iterator;

public class MDBDump {

public static Object [] [] jreadTable(Table table) {
		int count = table.getRowCount();
		
		Object [] [] data = new Object [count] [];
		Iterator it = table.iterator();
		
		for (int i=0;i<count;i++) {
			data[i] = (((Map) it.next()).values()).toArray();
		}
		return data;
	}

}
