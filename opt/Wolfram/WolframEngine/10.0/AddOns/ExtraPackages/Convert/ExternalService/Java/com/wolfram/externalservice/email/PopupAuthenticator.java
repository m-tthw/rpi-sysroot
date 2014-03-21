package com.wolfram.externalservice.email;

import javax.mail.*;
import com.wolfram.jlink.*;


public class PopupAuthenticator extends javax.mail.Authenticator 
{
	private String initUsername;
	
	public PopupAuthenticator(String initUsername) {
		super();
		this.initUsername = initUsername;
	}
	
	public javax.mail.PasswordAuthentication getPasswordAuthentication() 
	{
		PasswordAuthentication pswdauth = null;
		
		KernelLink ml = StdLink.getLink();
        StdLink.requestTransaction();
        synchronized (ml) {
            try {
                ml.putFunction("EvaluatePacket", 1);
                ml.putFunction("ExternalService`Utilities`loginDialog", 2);
                ml.put("");
                ml.put(this.initUsername);
                ml.endPacket();
                ml.waitForAnswer();
                Expr res = ml.getExpr();
                ml.newPacket();
                // In an asynchronous request, we get back a symbol that is later assigned to in
                // Mathematica. We poll its value to wait until it has been assigned.
                String resultSymbol = res.asString();
                // M sends back Null to indicate it will not try to display a dialog, and we should bail out.
                boolean finished = false;
                while (!finished) {
                    try { Thread.sleep(200); } catch (InterruptedException e) {}
                    ml.evaluate(resultSymbol);
                    ml.waitForAnswer();
                    res = ml.getExpr();
                    if (res.listQ()) {
                        pswdauth = new PasswordAuthentication(res.part(1).asString(), res.part(2).asString());
                        finished = true;
                    } else if (res.asString().equals("$Canceled")) {
                        finished = true;
                    }
                }
            } catch (Exception e) {
                ml.clearError();
                ml.newPacket();
            }
        }
		return pswdauth;
	}

}