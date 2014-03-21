package System.Convert;

import com.wolfram.jlink.*;

import java.io.*;
import java.util.Calendar;
import java.util.Date;
import jxl.*;
import jxl.write.*;
import jxl.write.Number;
import jxl.write.Boolean;


public class ExcelDump {

	static LoopbackLink loop;

	/* symbolic Exprs */
	static Expr $Failed = new Expr(Expr.SYMBOL, "$Failed");
	static Expr True = new Expr(Expr.SYMBOL, "True");
	static Expr False = new Expr(Expr.SYMBOL, "False");
	static Expr Null = new Expr(Expr.SYMBOL, "Null");
	static DateFormat customDateFormat = new DateFormat ("dd/MMM/yyyy"); 
	static WritableCellFormat dateFormat = new WritableCellFormat (customDateFormat); 

	static Workbook open(String fn) throws MathLinkException {
		KernelLink link = StdLink.getLink();
		Workbook workbook;
		try {
			File file = new File(fn);
			workbook = Workbook.getWorkbook(file);
		} catch (IOException e) {
			link.message("Import::fnfnd", fn);
			link.put($Failed);
			workbook = null;
		} catch (Exception e) {
			link.message("Import::fmterr", "XLS");
			link.put($Failed);
			workbook = null;
		}
		return workbook;
	}

	public static void getNames(String fn) throws MathLinkException {
		KernelLink link = StdLink.getLink();
		link.beginManual();

		Workbook workbook = open(fn);
		if (workbook == null)
			return;

		try {
			String[] names = workbook.getSheetNames();
			int num = names.length;

			link.putFunction("List", num);
			for (int i = 0; i < num; i++) {
				link.put(names[i]);
			}

		} catch (Exception e) {
			link.message("Import::fmterr", "XLS");
			link.put($Failed);
		} finally {
			workbook.close();
		}
	}

	public static void read(String fn, boolean Formulas)
			throws MathLinkException {
		KernelLink link = StdLink.getLink();
		link.beginManual();

		Workbook workbook = open(fn);
		if (workbook == null)
			return;

		try {
			int num = workbook.getNumberOfSheets();

			loop = MathLinkFactory.createLoopbackLink();
			loop.putFunction("List", num);
			for (int i = 0; i < num; i++) {
				if (link.wasInterrupted()) {
					workbook.close();
					return;
				}
				ReadSheet(workbook.getSheet(i), Formulas);
			}
			link.transferExpression(loop);

		} catch (Exception e) {
			link.message("Import::fmterr", "XLS");
			link.put($Failed);
		} finally {
			loop.close();
			workbook.close();
		}
	}

	public static void read(String fn, String name, boolean Formulas)
			throws MathLinkException {
		KernelLink link = StdLink.getLink();
		link.beginManual();

		Workbook workbook = open(fn);
		if (workbook == null)
			return;

		try {
			loop = MathLinkFactory.createLoopbackLink();

			Sheet sheet;
			sheet = workbook.getSheet(name);

			if (sheet == null) { /* invalid string */
				link.message("Import::noelem", name);
				link.put($Failed);
				loop.close();
				workbook.close();
				return;
			}
			ReadSheet(sheet, Formulas);

			link.transferExpression(loop);

		} catch (Exception e) {
			link.message("Import::fmterr", "XLS");
			link.put($Failed);
		} finally {
			loop.close();
			workbook.close();
		}
	}

	public static void read(String fn, int name, boolean Formulas)
			throws MathLinkException {
		KernelLink link = StdLink.getLink();
		link.beginManual();

		Workbook workbook = open(fn);
		if (workbook == null)
			return;

		try {
			int num = workbook.getNumberOfSheets();
			if (name < 0) /* treat negative values as nth from the last sheet */
				name = num + name;

			loop = MathLinkFactory.createLoopbackLink();

			Sheet sheet;
			try {
				sheet = workbook.getSheet(name);
			} catch (IndexOutOfBoundsException e) { /* invalid integer */
				link.message("Import::noelem", (new Integer(name + 1))
						.toString());
				link.put($Failed);
				loop.close();
				workbook.close();
				return;
			}
			ReadSheet(sheet, Formulas);

			link.transferExpression(loop);

		} catch (Exception e) {
			link.message("Import::fmterr", "XLS");
			link.put($Failed);
		} finally {
			loop.close();
			workbook.close();
		}
	}

	static void ReadSheet(Sheet sheet, boolean Formulas)
			throws MathLinkException {
		int num = sheet.getRows();

		loop.putFunction("List", num);
		for (int i = 0; i < num; i++)
			ReadRow(sheet.getRow(i), Formulas);
	}

	static void ReadRow(Cell[] row, boolean Formulas) throws MathLinkException {
		int num = row.length;

		loop.putFunction("List", num);
		for (int i = 0; i < num; i++)
			ReadCell(row[i], Formulas);
	}

	static void ReadCell(Cell cell, boolean Formulas) throws MathLinkException {
		CellType type = cell.getType();
		String s;
		if (Formulas) {
			if (type == CellType.NUMBER_FORMULA
					|| type == CellType.BOOLEAN_FORMULA
					|| type == CellType.STRING_FORMULA
					|| type == CellType.DATE_FORMULA) {
				try {
					FormulaCell fc = (FormulaCell) cell;
					s = fc.getFormula();
					loop.put(s);
				} catch (Exception e) {
					e.printStackTrace();
					loop.putSymbol("$Failed");
				}
			} else if (type == CellType.ERROR || type == CellType.FORMULA_ERROR) {
				loop.putSymbol("$Failed");
			} else {
				loop.putSymbol("Null");
			}

		} else {

			if (type == CellType.NUMBER || type == CellType.NUMBER_FORMULA) {
				NumberCell c = (NumberCell) cell;
				loop.put(c.getValue());
			} else if (type == CellType.BOOLEAN
					|| type == CellType.BOOLEAN_FORMULA) {
				BooleanCell c = (BooleanCell) cell;
				loop.put(c.getValue());
			} else if (type == CellType.DATE || type == CellType.DATE_FORMULA) {
				DateCell c = (DateCell) cell;
				Date date = c.getDate();

				/*
				 * ToDate[FromDate[d]] fixes any rollovers caused by
				 * TimezoneOffset
				 */
				loop.putFunction("ToDate", 1);
				loop.putFunction("FromDate", 1);
				loop.putFunction("List", 6);
				loop.put(date.getYear() + 1900);
				loop.put(date.getMonth() + 1);
				loop.put(date.getDate());
				loop.put(date.getHours() + date.getTimezoneOffset() / 60);
				loop.put(date.getMinutes());
				loop.put(date.getSeconds());
			} else if (type == CellType.EMPTY) {
				loop.putSymbol("Null");
			} else if (type == CellType.ERROR || type == CellType.FORMULA_ERROR) {
				loop.putSymbol("$Failed");
			} else { /* LABEL, STRING_FORMULA */
				loop.put(cell.getContents());
			}

		}
	}

	public static Expr write(String fn, String[] names, Expr[][][] data,
			Expr[][][] formulas) throws MathLinkException {
		KernelLink link = StdLink.getLink();
		try {

			File file = new File(fn);
			WritableWorkbook workbook = Workbook.createWorkbook(file);
			WritableSheet sheet = null;
			int num = data.length;
			int fnum = formulas.length;
			if (names.length != num) { /* should never happen */
				workbook.close();
				return $Failed;
			}

			for (int i = 0; i < num; i++) {
				if (link.wasInterrupted()) {
					workbook.write();
					workbook.close();
					return $Failed;
				}
				sheet = workbook.createSheet(names[i], i);
				WriteSheet(sheet, data[i]);
				if (i < fnum)
					WriteFormulaData(sheet, formulas[i]);
			}

			workbook.write();
			workbook.close();
			return new Expr(fn);
		} catch (IOException e) {
			link.message("Export::errfile", e.getMessage());
			return $Failed;
		} catch (WriteException e) {
			link.message("Export::fmterr", "XLS");
			return $Failed;
		}
	}

	static void WriteFormulaData(WritableSheet sheet, Expr[][] formulas)
			throws WriteException {
		int num = formulas.length;
		for (int i = 0; i < num; i++) {
			WriteFormulaRow(sheet, i, formulas[i]);
		}
	}

	static void WriteFormulaRow(WritableSheet sheet, int row, Expr[] formulas)
			throws WriteException {
		int num = formulas.length;
		for (int i = 0; i < num; i++)
			WriteFormulaCell(sheet, row, i, formulas[i]);
	}

	static void WriteFormulaCell(WritableSheet sheet, int row, int col,
			Expr formula) throws WriteException {

		if (formula.stringQ()) {
			try {
				Formula f = new Formula(col, row, formula.asString());
				sheet.addCell(f);
			} catch (Exception e) {
				e.printStackTrace();
			}
		}
	}

	static void WriteSheet(WritableSheet sheet, Expr[][] data)
			throws WriteException {
		int num = data.length;

		for (int i = 0; i < num; i++)
			WriteRow(sheet, i, data[i]);
	}

	static void WriteRow(WritableSheet sheet, int row, Expr[] data)
			throws WriteException {
		int num = data.length;

		for (int i = 0; i < num; i++)
			WriteCell(sheet, row, i, data[i]);
	}

	static void WriteCell(WritableSheet sheet, int row, int col, Expr data)
			throws WriteException {
		/*
		 * N is applied to data before it is sent, so we can assume ints,
		 * rationals, and numeric symbolic expresssions are reals
		 */
		int d1,d2,d3,d4,d5,d6;
		if (data.realQ() && !data.bigDecimalQ()) {
			try {
				Number number = new Number(col, row, data.asDouble());
				sheet.addCell(number);
			} catch (ExprFormatException e) {
				WriteLabel(sheet, row, col, data);
			}
		} else if (data.equals(True)) {
			Boolean bool = new Boolean(col, row, true);
			sheet.addCell(bool);
		} else if (data.equals(False)) {
			Boolean bool = new Boolean(col, row, false);
			sheet.addCell(bool);
		} else if (data.equals(Null)) {
			/* skip the cell */
		} else if (data.listQ() && data.length() == 6) {      /* Added by kalpanat to process Dates */
				try {
								
					d1=(int) data.part(1).asDouble();
					d2=(int)data.part(2).asDouble();
					d3=(int)data.part(3).asDouble();
					d4=(int)data.part(4).asDouble();
					d5=(int)data.part(5).asDouble();
					d6=(int)data.part(6).asDouble();
					Calendar cldr = Calendar.getInstance();
					cldr.clear();
					cldr.set(d1, d2-1, d3, d4, d5, d6); /*Subtracting 1 from month d2 since Calendar's month start from 0*/
					Date dt = cldr.getTime(); 
					// Change these two variables to Static ones for the bug 125866.
//					DateFormat customDateFormat = new DateFormat ("dd/MMM/yyyy"); 
//					WritableCellFormat dateFormat = new WritableCellFormat (customDateFormat); 
					DateTime dateCell = new DateTime(col,row ,dt, dateFormat); 
					sheet.addCell(dateCell); 

					} 
					catch (ExprFormatException e) {
						
						WriteLabel(sheet, row, col, data);
					}
				
		} else {
			WriteLabel(sheet, row, col, data);
		}
	}

	static void WriteLabel(WritableSheet sheet, int row, int col, Expr data)
			throws WriteException {
		String str;
		try {
			str = data.asString();	
		} catch (ExprFormatException e) {
			str = data.toString();
			/* chop off quote characters */
			if (str.charAt(0) == '\"' && str.charAt(str.length()-1) == '\"') {
				str = str.substring(1, str.length() - 1);
			}
		}

		Label label = new Label(col, row, str);
		sheet.addCell(label);
	}

}