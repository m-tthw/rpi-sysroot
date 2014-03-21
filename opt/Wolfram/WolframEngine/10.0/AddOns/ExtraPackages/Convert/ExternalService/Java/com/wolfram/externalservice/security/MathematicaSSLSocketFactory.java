package com.wolfram.externalservice.security;

import java.io.IOException;
import java.net.InetAddress;
import java.net.Socket;
import java.net.SocketException;
import java.security.NoSuchAlgorithmException;

import javax.net.SocketFactory;
import javax.net.ssl.*;

import com.wolfram.externalservice.security.MathematicaTrustManager;

//import java.security.NoSuchAlgorithmException;
//import java.security.NoSuchProviderException;
//import java.security.SignatureException;
//import java.security.cert.CertificateException;
//import java.security.cert.X509Certificate;
//import java.security.KeyStore;
//import java.security.KeyStoreException;
import java.security.KeyManagementException;

/**
 * MathematicaSSLSocketFactory
 * 
 * Class for creating SSL sockets that use MathemticaTrustManager.java to 
 * manage server certificates.  
 * 
 * Set the system property "com.wolfram.externalservice.security.SSLProtocol"
 * before calling for a socket to get either a "TLS" or "SSL" socket.
 * 
 * @author lambertc
 */

public class MathematicaSSLSocketFactory extends SSLSocketFactory {
    private SSLSocketFactory SSLFactory, TLSFactory;

    public MathematicaSSLSocketFactory() 
    	throws SSLException{
    	try {
            TLSFactory = createSSLProtocolSocketFactory("TLS");
            SSLFactory = createSSLProtocolSocketFactory("SSL");
        } catch(SSLException sslExc) {
        	throw sslExc;
        }
    }
    
    private SSLSocketFactory createSSLProtocolSocketFactory (String SSLProtocol) 
    	throws SSLException {
    	SSLSocketFactory sslSocketFactory = null;
    	try {
    		SSLContext sslcontext = SSLContext.getInstance(SSLProtocol);
    		MathematicaTrustManager tm = new MathematicaTrustManager();
    		sslcontext.init(null, new TrustManager[] {tm}, null);
    		sslSocketFactory =  sslcontext.getSocketFactory();
    	} catch (MathematicaTrustException mtExc) {
    		throw new SSLException("Could not create socket factory: MathematicaTrustManager error.", mtExc);
    	} catch (NoSuchAlgorithmException nsaExc) {
    		throw new SSLException("Could not get SSL context: No such algorithm", nsaExc);
    	} /*catch (IOException ioExc){
    		
    	} */ catch (KeyManagementException kmExc){
    		
    	}
    	
    	return sslSocketFactory;
    }
    
    public static SocketFactory getDefault() {
        SocketFactory sf = null;
        try{
        	sf = new MathematicaSSLSocketFactory();
        } catch (SSLException sslExc) {
        	return null;
        }
       	return sf;
    }

    public Socket createSocket() throws IOException, SocketException {
        if (System.getProperty("com.wolfram.externalservice.security.SSLProtocol").equals("TLS"))
    		return TLSFactory.createSocket();
        else
        	return SSLFactory.createSocket();
    }

    public Socket createSocket(Socket socket, String s, int i, boolean flag)
                                throws IOException, SocketException {
    	if (System.getProperty("com.wolfram.externalservice.security.SSLProtocol").equals("TLS"))
        	return TLSFactory.createSocket(socket, s, i, flag);
        else 
        	return SSLFactory.createSocket(socket, s, i, flag);
    }

    public Socket createSocket(InetAddress inaddr, int i,
                                InetAddress inaddr1, int j) throws IOException, SocketException {
        if (System.getProperty("com.wolfram.externalservice.security.SSLProtocol").equals("TLS"))
        	return TLSFactory.createSocket(inaddr, i, inaddr1, j);
        else
        	return SSLFactory.createSocket(inaddr, i, inaddr1, j);
    }

    public Socket createSocket(InetAddress inaddr, int i)
                                throws IOException, SocketException {
        if (System.getProperty("com.wolfram.externalservice.security.SSLProtocol").equals("TLS"))
        	return TLSFactory.createSocket(inaddr, i);
        else
        	return SSLFactory.createSocket(inaddr, i);
    }

    public Socket createSocket(String s, int i, InetAddress inaddr, int j)
                                throws IOException, SocketException {
    	if (System.getProperty("com.wolfram.externalservice.security.SSLProtocol").equals("TLS"))
        	return TLSFactory.createSocket(s, i, inaddr, j);
        else
        	return SSLFactory.createSocket(s, i, inaddr, j);
    }

    public Socket createSocket(String s, int i) throws IOException, SocketException {
        if (System.getProperty("com.wolfram.externalservice.security.SSLProtocol").equals("TLS"))
        	return TLSFactory.createSocket(s, i);
        else
        	return SSLFactory.createSocket(s, i);
    }

    public String[] getDefaultCipherSuites() {
        if (System.getProperty("com.wolfram.externalservice.security.SSLProtocol").equals("TLS"))
        	return TLSFactory.getDefaultCipherSuites();
        else
        	return SSLFactory.getDefaultCipherSuites();
    }

    public String[] getSupportedCipherSuites() {
        if (System.getProperty("com.wolfram.externalservice.security.SSLProtocol").equals("TLS"))
        	return TLSFactory.getSupportedCipherSuites();
        else
        	return SSLFactory.getSupportedCipherSuites();
    }
    
}