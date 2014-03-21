package com.wolfram.externalservice.security;

import java.security.Security;
import com.sun.net.ssl.internal.ssl.Provider;

import java.io.File;
import java.io.FileInputStream;
import java.io.FileOutputStream;
import java.io.IOException;
import java.nio.channels.FileChannel;

public class MathematicaSSLConfiguration{
	
	private static String defaultSvrAuth = null;
	
	public static void setSSLProperties(
			String defaultSSLSocketFactory,
			String trustStore,
			String trustStorePW,
			String defaultServerAuthentication
		){
		Security.setProperty("ssl.SocketFactory.provider", defaultSSLSocketFactory);
		if(defaultSSLSocketFactory.equals("com.wolfram.externalservice.MathematicaSSLSocketFactory") && 
				Security.getProvider("SunJSSE") == null) 
			Security.addProvider(new Provider());

		// Set TrustManager
		System.setProperty("javax.net.ssl.trustStore", trustStore);
		System.setProperty("javax.net.ssl.trustStorePassword", trustStorePW);
		System.setProperty("com.wolfram.externalservice.security.trustManager", defaultServerAuthentication);
		MathematicaSSLConfiguration.defaultSvrAuth = defaultServerAuthentication;
	}
	
	public static void setSSLProperties(
			String defaultSSLSocketFactory,
			String trustStore,
			String trustStorePW,
			String defaultServerAuthentication,
			String sourceTrustStore
		) throws IOException {
		MathematicaSSLConfiguration.setSSLProperties(
				defaultSSLSocketFactory,
				trustStore,
				trustStorePW,
				defaultServerAuthentication
			);
		MathematicaSSLConfiguration.checkCopy(sourceTrustStore, trustStore);
	}
	
	/*
	 * Sets the serverAuthentication system property but
	 * not default static field defaultSvrAuth
	 */ 
	public static void setServerAuthentication(String serverAuthentication) {
		System.setProperty("com.wolfram.externalservice.security.trustManager", serverAuthentication);
	}
	
	/*
	 * Returns the current setting for ServerAuthentication behavior
	 * (i.e, the system property setting)
	 */
	
	public static String getServerAuthentication() {
		return System.getProperty("com.wolfram.externalservice.security.trustManager");
	}
	
	/*
	 * Sets both the serverAuthentication system property and
	 * the default static field defaultSvrAuth
	 */
	public static void setDefaultServerAuthentication(String defaultServerAuthentication) {
		MathematicaSSLConfiguration.defaultSvrAuth = defaultServerAuthentication;
		System.setProperty("com.wolfram.externalservice.security.trustManager", MathematicaSSLConfiguration.defaultSvrAuth);
	}
	
	/*
	 * Forces default Server Authentication behavior
	 */
	public static void useDefaultServerAuthentication() 
		throws NullPointerException {
		if (MathematicaSSLConfiguration.defaultSvrAuth == null) 
			throw new NullPointerException("Default ServerAuthentication value has not been set.");
		System.setProperty("com.wolfram.externalservice.security.trustManager", MathematicaSSLConfiguration.defaultSvrAuth);
	}
	
	
	
	/* Returns actual values of system/security properties
	 * These properties may not reflect what the system is actually using, e.g.,
	 * changing the security property "ssl.SocketFactory.provider" has no effect
	 * if a default SSLSocketFactory has already been instantiated.
	 * 
	 * Returns default global setting for trustManager behavior (not current setting)
	 */
	public static String[] getSSLProperties() {
		String[] config = {
			Security.getProperty("ssl.SocketFactory.provider"),
			System.getProperty("javax.net.ssl.trustStore"),
			System.getProperty("javax.net.ssl.trustStorePassword"),
			MathematicaSSLConfiguration.defaultSvrAuth
		};
		return config;
	}
	
	// copies source to target if target does not exist
	// creates intermediate directories
	private static void checkCopy(String sourceFileName, String targetFileName)
	throws IOException {
		// Check target exists
		File targetFile = new File(targetFileName);
		if (!targetFile.exists()){
			File sourceFile = new File(sourceFileName);
			if (!sourceFile.exists()) 
				throw new IOException("Source file does not exist.");
			
			// check target directories
			File targetDirFile = targetFile.getParentFile();
			if (!targetDirFile.exists()) targetDirFile.mkdirs();
			
			// start copying
			FileChannel sourceFCh = null;
			FileChannel targetFCh = null;
			try {
				sourceFCh = new FileInputStream(sourceFile).getChannel();
				targetFCh = new FileOutputStream(targetFile).getChannel();
				sourceFCh.transferTo(0, sourceFCh.size(), targetFCh);
				sourceFCh.close();
				targetFCh.close();
			} catch (IOException e) {
				throw e;
			} 
		}
	}
	
}
