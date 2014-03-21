package com.wolfram.externalservice.security;

import java.security.NoSuchAlgorithmException;
import java.security.NoSuchProviderException;
import java.security.cert.CertificateException;
import java.security.cert.X509Certificate;
import java.security.KeyStore;
import java.security.KeyStoreException;
import java.io.File;
import java.io.FileInputStream;
import java.io.FileOutputStream;
import java.io.FileNotFoundException;
import java.io.IOException;

import javax.net.ssl.TrustManager;
import javax.net.ssl.X509TrustManager;
import javax.net.ssl.TrustManagerFactory;

import com.wolfram.jlink.*;

import javax.naming.InvalidNameException;
import javax.naming.ldap.LdapName;

/**
 * MathematicaTrustManager
 * 
 * Class implements Mathematica's trust manager.  It delegates trust decisions
 * to the default X509TrustManager returned by SunX509.  When that fails, it 
 * then either
 * 	1) TRUST_STORE_ONLY: accepts this decision
 *  2) ALLOW: accepts all certificates anyway
 *  3) ASK: pops up a dialog box allowing you to 
 *  	a) accept the certificate (ALLOW_TEMPORARILY)
 *  	b) accept the certificate and add it to the truststore (ALLOW_PERMANENTLY)
 *  	c) not accept the certificate (REJECT)
 *  
 *  Set behavior 1, 2, 3 via the "com.wolfram.externalservice.security.trustManager"
 *  system property before any calls to the trustmanager.
 *  
 *  When the constructor is called, it uses the truststore and password set in
 *  the system properties:
 *  	"javax.net.ssl.trustStore"
 *  	"javax.net.ssl.trustStorePassword"
 * 
 * @author lambertc
 */

public class MathematicaTrustManager implements X509TrustManager {

    /*
     * The default X509TrustManager returned by SunX509.  We'll delegate
     * decisions to it, and fall back to the logic in this class if the
     * default X509TrustManager doesn't trust it.
     */
    X509TrustManager sunJSSEX509TrustManager;
    public final static int TRUST_STORE_ONLY = 0;
    public final static int ASK = 1;
    public final static int ALLOW = 2;
    public final static int ALLOW_TEMPORARILY = 0;
    public final static int ALLOW_PERMANENTLY = 1;
    public final static int REJECT = 0;
    
    private static int trustManagerBehavior;

    /* 
     * Keystore in memory
     * Necessary because tmf.init copies in constructor copies over the loaded
     * keystore, and we can't add to it.
     */
    private static KeyStore ks;  // keystore in memory; 
    
    MathematicaTrustManager() 
    	throws /*KeyStoreException, NoSuchProviderException, NoSuchAlgorithmException, CertificateException, FileNotFoundException, IOException,*/ 
    		MathematicaTrustException {
        
    	// create a "default" JSSE X509TrustManager.
    	try{
    		MathematicaTrustManager.ks = KeyStore.getInstance("JKS");
    	} catch (KeyStoreException ksExc){
    		throw new MathematicaTrustException("Could not instantiate keystore", ksExc);
    	}
        
    	// instantiate trust manger factory
    	TrustManagerFactory tmf = null;
    	
    	try{
    		tmf = TrustManagerFactory.getInstance("SunX509", "SunJSSE");
    	} catch (NoSuchProviderException nspExc) {
    		throw new MathematicaTrustException("Could not instantiate trust manager factory.", nspExc);
    	} catch (NoSuchAlgorithmException nsaExc){
    		throw new MathematicaTrustException("Could not instantiate trust manager factory.", nsaExc);
    	}
    	
    	// load keystore and intialize tmf
        String trustStoreFile = System.getProperty("javax.net.ssl.trustStore");
        String trustStorePassword = System.getProperty(
        		"javax.net.ssl.trustStorePassword");
        if (trustStoreFile != null && trustStorePassword != null)
        {
        	FileInputStream keyStoreFIS = null;
        	try{
        		keyStoreFIS = new FileInputStream(trustStoreFile);
        	} catch (FileNotFoundException fnfExc){
        		try{
        			MathematicaTrustManager.loadNullKeyStore();
        		} catch (MathematicaTrustException mtExc) {
        			throw mtExc;
        		}
        	}
        	try{
        		MathematicaTrustManager.ks.load(keyStoreFIS,
                        trustStorePassword.toCharArray());
        	} catch (IOException ioExc) {
        		throw new MathematicaTrustException("Error loading trust store.", ioExc);
        	} catch (NoSuchAlgorithmException nsaExc){
        		throw new MathematicaTrustException("Error loading trust store.", nsaExc);
        	} catch (CertificateException cExc){
        		throw new MathematicaTrustException("Error loading trust store.", cExc);
        	} finally {
        		try{
        			keyStoreFIS.close();
        		} catch (IOException ioExc){
        			throw new MathematicaTrustException("Error closing trust store.", ioExc);
        		}
        	}
        }
        else {
        	try{
        		loadNullKeyStore();
        	} //This exception should not be thrown with a null keystore
        	catch (MathematicaTrustException mtExc) {
        		throw mtExc;
        	} 
        }
        
        try{
        	tmf.init(MathematicaTrustManager.ks);
        } catch (KeyStoreException ksExc){
        	throw new MathematicaTrustException("Could not initialize trust manager factory with keystore", ksExc);
        }
       
        TrustManager tms [] = tmf.getTrustManagers();

        /*
         * Iterate over the returned trustmanagers, look
         * for an instance of X509TrustManager.  If found,
         * use that as our "default" trust manager.
         */
        for (int i = 0; i < tms.length; i++) {
            if (tms[i] instanceof X509TrustManager) {
                sunJSSEX509TrustManager = (X509TrustManager) tms[i];
                return;
            }
        }
        
        // if we haven't returned, then we were unable to initialize
        throw new MathematicaTrustException(
        		"Could not initialize MathematicaTrustManager.");
    }

    /*
     * checkClientTrusted 
     * Delegates to the default trust manager.
     */
    public void checkClientTrusted(X509Certificate[] chain, String authType)
                throws CertificateException {
    	try {
            sunJSSEX509TrustManager.checkClientTrusted(chain, authType);
        } catch (CertificateException cExc) {
        	throw (cExc);
        }
    }

    /*
     * checkServerTrusted
     * Delegates to the default trust manager.  If it fails, 
     * then behavior is determined by this.TrustManagerBehavior
     */
    public void checkServerTrusted(X509Certificate
    		[] chain, String authType)
                throws CertificateException { 
    	
    	try {
            sunJSSEX509TrustManager.checkServerTrusted(chain, authType);
        } 
        catch (CertificateException cExc) 
        {
        	try {
        		//  certificate is in memory ks
        		X509Certificate storedCert = (X509Certificate) MathematicaTrustManager.ks.getCertificate(
						getCN(chain[0].getSubjectDN().toString()));
        		if( storedCert != null && // expect ks alias to be domain name
        				storedCert.getSubjectX500Principal().equals(
        						chain[0].getSubjectX500Principal()) &&
        				storedCert.getIssuerX500Principal().equals(
        						chain[0].getIssuerX500Principal())
            		) return;
			} catch (KeyStoreException ksExc) {
				throw new CertificateException(
						"Could not verify certificate: Truststore error.", ksExc);
			}
			
			// If certificate not in memory keystore, then keep checking:
			// Get behavior from system property: Ask, Allow, TrustStoreOnly
        	MathematicaTrustManager.trustManagerBehavior = 
        		Integer.parseInt(System.getProperty(
        				"com.wolfram.externalservice.security.trustManager"));
        	
        	if( // Allow
        		MathematicaTrustManager.trustManagerBehavior == (MathematicaTrustManager.ALLOW)) {
        		return;
        	} else {
        		if( //Ask
        			MathematicaTrustManager.trustManagerBehavior == (MathematicaTrustManager.ASK))
        		{
        			try {
        				chooseCertificateBehavior(chain);
        			}
        			catch(CertificateException chooseExc){  // Do not allow
        				throw cExc;
        			}
        			catch(KeyStoreException ksExc){ // Exception adding to keystore
        				// throw new CertificateException("Could not verify certificate.  Truststore error.", ksExc);
        				
        				/* 
        				 * chooseCertificateBehavior throws ksExc when it can't add truststore to memory.
        				 * Fail silently, so execution can continue
        				 */
        			}
        		} else // TRUST_STORE_ONLY
        			throw cExc;
        	}
        	
        }
    }

    /*
     * Merely pass this through.
     */
    public X509Certificate[] getAcceptedIssuers() {
        return sunJSSEX509TrustManager.getAcceptedIssuers();
    }
    
    private static void loadNullKeyStore()
		throws MathematicaTrustException {
		try{
			MathematicaTrustManager.ks.load(null);
		} //These exceptions should not be thrown with a null keystore
		catch (IOException ioExc) {
			throw new MathematicaTrustException("Error loading trust store.", ioExc);
		} catch (NoSuchAlgorithmException nsaExc){
			throw new MathematicaTrustException("Error loading trust store.", nsaExc);
		} catch (CertificateException cExc){
			throw new MathematicaTrustException("Error loading trust store.", cExc);
		}
    }
    
    
    private static void chooseCertificateBehavior(X509Certificate[] chain)
    	throws CertificateException, KeyStoreException
    {
    	String s = null;
    	KernelLink ml = StdLink.getLink();
		StdLink.requestTransaction();

        synchronized (ml) {
        	try{
        		ml.putFunction("EvaluatePacket", 1);
        		ml.putFunction("ExternalService`Utilities`certificateDialog", 5);
        		ml.put(getCN(chain[0].getSubjectDN().toString()));
        		ml.put(splitX500PrincipalString(chain[0].getSubjectX500Principal().getName("RFC2253")));
        		ml.put(chain[0].getSerialNumber());
        		ml.put(splitX500PrincipalString(chain[0].getIssuerX500Principal().getName("RFC2253")));
        		String[] validDate = {chain[0].getNotBefore().toString(),chain[0].getNotAfter().toString()};
        		ml.put(validDate);
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
                    if (res.stringQ()) {
                        s = res.asString();
                        finished = true;
                    }
                }
            } catch (Exception e) {
                ml.clearError();
                ml.newPacket();
            }
    	} 
    	
    	String alias = getCN(chain[0].getSubjectDN().toString());
		X509Certificate cert = chain[0];
    	
		// Cancel or close dialog box
		if(s == null) throw new CertificateException(
			"Unable to verify identity of server as trusted." );
		
    	if(s.equals("Accept permanently"))
    	{
    		try{
    			MathematicaTrustManager.ks.setCertificateEntry(alias, cert);
    			addToKeyStoreFile(new File(System.getProperty("javax.net.ssl.trustStore")),
    					(System.getProperty("javax.net.ssl.trustStorePassword")).toCharArray(), alias, cert);
    		} catch (Exception exc) {
    			// fail silently if could not write to keystore, so execution can continue
    			// throw new CertificateException("Could not write to trust store file.", exc);
    			// e.printStackTrace();
    		}
    				
    	}
    	else {
    		if(s.equals("Accept for this kernel session only"))
    		{
    			// Add to truststore in memory
    			try {
    				MathematicaTrustManager.ks.setCertificateEntry(alias, cert);
				} catch (KeyStoreException ksExc) {
					// fail silently if could not write to keystore, so execution can continue
	    			// throw new CertificateException("Could not write to trust store file.", ksExc);
	    			// e.printStackTrace();
				}
    		}
    		else  // "Do not accept"
    		{
    			throw new CertificateException(
    					"Unable to verify identity of server as trusted." );
    		}
    	}
    }
    
    private static void addToKeyStoreFile(File keystoreFile, char[] keystorePassword,
            String alias, X509Certificate cert) 
    	throws CertificateException, NoSuchAlgorithmException, KeyStoreException, IOException {
    	try {
           // Create an empty keystore object
           KeyStore keystore = KeyStore.getInstance(KeyStore.getDefaultType());
   
           // Load the keystore contents
           FileInputStream in = new FileInputStream(keystoreFile);
           keystore.load(in, keystorePassword); //throws CertificateException, IOException, NoSuchAlgorithmException
           in.close();
   
           // Add the certificate
           keystore.setCertificateEntry(alias, cert);
   
           // Save the new keystore contents
           FileOutputStream out = new FileOutputStream(keystoreFile);
           keystore.store(out, keystorePassword);  //throws CertificateException, IOException, NoSuchAlgorithmException, KeyStoreException
           out.close();
       } catch (CertificateException cExc) {
    	   throw cExc;
       } catch (NoSuchAlgorithmException nsaExc) {
    	   throw nsaExc;
       } catch (KeyStoreException ksExc) {
    	   // thrown by keystore.store if keystore failed to load
    	   throw ksExc;
       } catch (IOException ioExc) {
    	   throw ioExc;
       }
   }
    
    public static String[][] splitX500PrincipalString (String principal) 
    {
    	String tmpString;
    	String[][] parsedString;
    	LdapName ldn = null;
    	
    	
    	try {
			ldn = new LdapName(principal);
		} catch (InvalidNameException e) {
			e.printStackTrace();
		}
		
		parsedString = new String[ldn.size()][2];
		
    	for(int i = 0; i < ldn.size(); i++) {
    		tmpString = ldn.getRdn(i).toString().replace("\\,", ",");
    		parsedString[i] = tmpString.split("=");
    	}
    	return parsedString;
    }
    
    private static String getCN(String principal)
    {
    	for(String[] attribute : splitX500PrincipalString(principal))
    	{
    		if(attribute[0].equals("CN")) return attribute[1];
    	}
    	return null;
    }
}
       