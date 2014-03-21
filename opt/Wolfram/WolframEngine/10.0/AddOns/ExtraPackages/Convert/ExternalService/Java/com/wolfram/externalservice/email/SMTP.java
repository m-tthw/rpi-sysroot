package com.wolfram.externalservice.email;

import java.io.UnsupportedEncodingException;
import java.util.*;
import javax.mail.*;
import javax.mail.internet.*;

import java.util.Date;

import javax.activation.DataHandler;
import javax.activation.DataSource;
import javax.activation.FileDataSource;

import com.sun.mail.smtp.SMTPTransport;

/**
 * SMTP
 * 
 * Class implements java code for Mathematica's SendMail.  
 * 
 * Sets JVM system property for the socks proxy. This is a restriction from
 * JavaMail which follows Java 1.4 networking standards.
 * 
 * @author lambertc
 */

public class SMTP {
	
	private	boolean	debug;
	private boolean sendPartial;
	private	String	mailProtocol;
	private	String	encryptionProtocol;
	
	private	String	host;
	private	int	port;
	private	boolean	authenticate;
	private	String	socksProxyHost;
	private	int	socksProxyPort;
	
	private	String	username;
	private String	password;
	private String 	messageID;
	
	/* 
	 * timeouts (in milliseconds)
	 * negative integers for default values (CONNECT_TIMEOUT, SOCKET_IO_TIMEOUT)
	 * 0 for infinite
	 * positive for actual values
	 */
	
	private	int		connectTimeout = -1;
	private	int		socketIOTimeout = -1;
	
	private static final int CONNECT_TIMEOUT = 30000; // milliseconds
	private static final int SOCKET_IO_TIMEOUT = 30000; // milliseconds
	
	public	static	int		lastServerResponseCode = -1;
	public	static	String	lastServerResponse = null;
	
	/*
	 * Constructor arguments
	 * 
	 * String	mailProtocol	mail protocol: "smtp", "smtps"
	 * String	encryptionProtocol	SSL protocol: null, 
	 * 					"STARTTLS", "SSL", "TLS", "SSLv2", "SSLv3", "TLSv1"
	 * String	host		mailserver name: "mymailserver.com"
	 * int 		port		port number
	 * String	socksProxyHost	name of socks proxy server
	 * int		socksProxyPort	socks proxy port number
	 * boolean	authenticate	whether to do password authentication
	 * String	username	use null (for username and password) for popup window
	 * String	password	use null (for username and password) for popup window
	 * boolean	debug		Prints debugging information to console
	 * String	messageID	message ID for email
	 */
	
	public SMTP(String mailProtocol, String encryptionProtocol, 
			String host, int port, String socksProxyHost, 
			int socksProxyPort, boolean authenticate, String username, 
			String password, boolean debug, boolean sendPartial, String messageID){
		this.mailProtocol = mailProtocol;
		this.encryptionProtocol = encryptionProtocol;
		
		this.host = host;
		this.port = port;
		this.socksProxyHost = socksProxyHost;
		this.socksProxyPort = socksProxyPort;
				
		this.authenticate = authenticate;
		this.username = username;
		this.password = password;
		this.messageID = messageID;
		
		this.debug = debug;
		this.sendPartial = sendPartial;
	}
	
	public SMTP(String mailProtocol, String encryptionProtocol,  
			String host, int port, boolean authenticate, String username, 
			String password, boolean debug, boolean sendPartial, String messageID){
		this.mailProtocol = mailProtocol;
		this.encryptionProtocol = encryptionProtocol;
		
		this.host = host;
		this.port = port;
		this.socksProxyHost = null;
						
		this.authenticate = authenticate;
		this.username = username;
		this.password = password;
		this.messageID = messageID;
		
		this.debug = debug;
		this.sendPartial = sendPartial;
	}
	
	public void setConnectTimeout(int timeout){
		this.connectTimeout = timeout;
	}
	
	public void setSocketIOTimeout(int timeout){
		this.socketIOTimeout = timeout;
	}
	
	public int getConnectTimeout(){
		return this.connectTimeout;
	}
	
	public int getSocketIOTimeout() {
		return this.socketIOTimeout;
	}
	
	public String[] sendMail(String[] from, String[] replyTo, String[] to, 
			String[] cc, String[] bcc, String subject, String text, 
			String[][] attachments) 
	throws  /*IOException,*/ AddressException, SendFailedException, 
		AuthenticationFailedException, MessagingException, 
		NoSuchProviderException {
		
		Properties props = new Properties();
		
	// debug logging
		if(this.debug) {
			System.out.println(
				"\n\n-----------------------------------------------\n" + 
				"Mathematica SendMail Debug Mode: \n");
			// debug javamail
			props.put("mail.debug", "true");
//			System.setProperty("javax.activation.debug", "true");  // debug JAF
			// debug SSL
//			System.setProperty("java.protocol.handler.pkgs", 
//					"com.sun.net.ssl.internal.www.protocol");  
//			System.setProperty("javax.net.debug","all");
			// debugging certificates
//			System.setProperty("java.security.debug", "certpath");
//			System.setProperty("javax.net.debug", "trustmanager");
		}
		else {
			props.put("mail.debug", "false");  //debug javamail
//			System.setProperty("javax.activation.debug", "false");  // debug JAF
			// debug SSL
//			System.setProperty("java.protocol.handler.pkgs", "false");
//			System.setProperty("javax.net.debug","false");
			// debugging certificates
//			System.setProperty("java.security.debug", null);
//			System.setProperty("javax.net.debug", null);

		}
		
	// SOCKS proxy
		if(this.socksProxyHost != null) {
			System.setProperty("proxySet", "true");
			System.setProperty("socksProxyHost", this.socksProxyHost);
			System.setProperty("socksProxyPort", 
					Integer.toString(this.socksProxyPort));
		} else {
			System.setProperty("proxySet", "false");
			System.setProperty("socksProxyHost","");
			System.setProperty("socksProxyPort","0");
		}
		
		
	// authentication settings 
	//(use authenticator only when incomplete credentials are provided)
		javax.mail.Authenticator auth = null;
		if (this.authenticate && (this.username == null || this.password == null))
			auth =  new PopupAuthenticator(this.username);
			
	// Session properties
		Session session = null;
		props.put("mail." + this.mailProtocol + ".auth", 
				String.valueOf(this.authenticate));
		props.put("mail.transport.protocol", this.mailProtocol);
		props.put("mail." + this.mailProtocol + ".host", this.host);
		props.put("mail." + this.mailProtocol + ".port", Integer.toString(this.port));
		props.put("mail." + this.mailProtocol + ".from", from);
		props.put("mail." + this.mailProtocol + ".sendpartial", Boolean.toString(this.sendPartial));
		// Do NOT throw SMTPAddressSucceededException; this would confuse
		// Mathematica-side exception handling
		props.put("mail." + this.mailProtocol + ".reportsuccess", false);
		
		// Timeouts
		if(this.connectTimeout < 0){
			props.put("mail." + this.mailProtocol + ".connectiontimeout", Integer.toString(SMTP.CONNECT_TIMEOUT));
		} else {
			if (this.connectTimeout == 0)
				props.put("mail." + this.mailProtocol + ".connectiontimeout", "");
			else
				props.put("mail." + this.mailProtocol + ".connectiontimeout", Integer.toString(this.connectTimeout));
		}
		
		if (this.socketIOTimeout < 0) {
			props.put("mail." + this.mailProtocol + ".timeout", Integer.toString(SMTP.SOCKET_IO_TIMEOUT));
		} else {
			if (this.socketIOTimeout == 0) {
				props.put("mail." + this.mailProtocol + ".timeout", "");
			} else
				props.put("mail." + this.mailProtocol + ".timeout", Integer.toString(this.socketIOTimeout));
		}
		
		if(this.encryptionProtocol != null) {
			if(this.mailProtocol.equals("smtp") && 
					this.encryptionProtocol.equals("STARTTLS")) 
				props.put("mail.smtp.starttls.enable", "true");
			if ((this.mailProtocol.equals("smtp") && 
					this.encryptionProtocol.equals("STARTTLS")) || 
					this.mailProtocol.equals("smtps"))
			{
				// SSL settings
				// Provider added in ExternalService`SSLInitialize[] on  M-side
//				Security.addProvider( new com.sun.net.ssl.internal.ssl.Provider());
				
				// Default SSL Socket Factory
//				final String SSL_FACTORY = "javax.net.ssl.SSLSocketFactory";  
				final String SSL_FACTORY = "com.wolfram.externalservice.security.MathematicaSSLSocketFactory";
				props.put("mail." + this.mailProtocol + "socketFactory.class", SSL_FACTORY);  
				
				String translatedEncrypProtocol = null;
				
				if(this.encryptionProtocol.equals("STARTTLS")){
					translatedEncrypProtocol = "TLS";
				} else {
					translatedEncrypProtocol = this.encryptionProtocol;
				}
				System.setProperty("com.wolfram.externalservice.security.SSLProtocol", translatedEncrypProtocol);
				
				// default SSL Socket Factory set by M function: ExternalService`SSLInitialize[]
//				java.security.Security.setProperty("ssl.SocketFactory.provider", SSL_FACTORY);
			}
		}
		
		// session will have an authenticator only if authenticate = true and 
		// 	incomplete credentials are provided
		session = 
			(this.authenticate && (this.username==null || this.password==null)) ? 
				Session.getInstance(props, auth) : Session.getInstance(props);
		
		SMTPTransport transportSMTP = null;

		transportSMTP = (SMTPTransport) session.getTransport();
	    
	    MimeMessage msg = buildMimeMessage(session, from, replyTo, to, 
				cc, bcc, subject, text, /*MimeType,*/ attachments, this.messageID);
		//throws AddressException, MessagingException
	    
	    int lastReturnCode;
	    String lastServerResponse = null;
	    String[] responseArray = new String[2];
	    try {
			if (this.authenticate && this.username != null && this.password != null)
				transportSMTP.connect(this.username, this.password);  
			else 
				transportSMTP.connect(); 
			transportSMTP.sendMessage(msg, msg.getAllRecipients());  
				//throws MessagingException, SendFailedException 
		} 
		finally {
			lastReturnCode = transportSMTP.getLastReturnCode();
			lastServerResponse = transportSMTP.getLastServerResponse();
			transportSMTP.close();
			responseArray[0] = Integer.toString(lastReturnCode); 
			responseArray[1] = lastServerResponse;
   		}
		
		return(responseArray);
   }
	
	private static MimeMessage buildMimeMessage(Session session, String[] from, 
			String[] replyTo, String[] to, String[] cc, String[] bcc, 
			String subject, String text, String[][] attachments, String messageID) 
		throws AddressException, MessagingException {
		
		if(from == null) 
			throw new AddressException("No from address provided.");
		if(to == null && cc == null && bcc == null) 
			throw new AddressException("No recipient addresses provided.");
		if (subject == null) subject = "";
		if (text == null) text = "";
		
//		MimeMessage msg = new MimeMessage(session);
		MimeMessage msg = new SendMailMimeMessage(session, messageID);
		
		InternetAddress fromAddress = new InternetAddress(from[0], true);
		try {
			fromAddress.setPersonal(from[1], "UTF-8");
		} catch (UnsupportedEncodingException e) {
			// Encoding is supported
			e.printStackTrace();
		}
		
		msg.setFrom(fromAddress);
		if(replyTo != null) msg.setReplyTo(buildInternetAddressArray(replyTo));
		if(to != null) 
			msg.setRecipients(Message.RecipientType.TO, buildInternetAddressArray(to));
		if(cc != null) 
			msg.setRecipients(Message.RecipientType.CC, buildInternetAddressArray(cc));
		if(bcc != null) 
			msg.setRecipients(Message.RecipientType.BCC, buildInternetAddressArray(bcc));
			
		msg.setSubject(subject, "UTF-8");
		msg.setSentDate(new Date());
		
	//	Message text
//		
//		MimeBodyPart msgBodyPart = new MimeBodyPart();
//		msgBodyPart.setText(text);
//		Multipart multipart = new MimeMultipart();
//		multipart.addBodyPart(msgBodyPart);
		
	//	Attachment handling
		if(attachments != null) {
			MimeBodyPart msgBodyPart = new MimeBodyPart();
			msgBodyPart.setText(text,"UTF-8");
			Multipart multipart = new MimeMultipart();
			multipart.addBodyPart(msgBodyPart);
			for(int i = 0; i < attachments.length; i++)
				  multipart.addBodyPart(addAttachment(attachments[i][1], attachments[i][0]));
			msg.setContent(multipart);
		} else {
			msg.setText(text, "UTF-8");
		}
		
		
		return msg;
	}
	
	private static InternetAddress[] buildInternetAddressArray(String[] addressArray)
		throws AddressException {
		InternetAddress[] internetAddressArray;
		
		internetAddressArray = new InternetAddress[addressArray.length];
		for (int i = 0; i < addressArray.length; i++) {
			internetAddressArray[i] = new InternetAddress(addressArray[i], true);
		}
		
		return internetAddressArray;
		
	}
	
	private static  MimeBodyPart addAttachment(String attachment, String contDisp) 
		throws MessagingException {
	 
	    MimeBodyPart messageBodyPart = new MimeBodyPart();
	    if (contDisp.equals("text")) {
	    	messageBodyPart.setText(attachment,"UTF-8");
	    } else {
	    	DataSource source = new FileDataSource(attachment);
		    messageBodyPart.setDataHandler(new DataHandler(source));
		    messageBodyPart.setFileName(source.getName());
		    messageBodyPart.setDisposition(contDisp);
	    }
	    return messageBodyPart;
	}
	
	public static InternetAddress createEmailAddress(String address)
		throws AddressException {
		return new InternetAddress(address, true);
	}
	
	public static InternetAddress createEmailAddress(String address, String personal)
		throws AddressException {
		InternetAddress addr = new InternetAddress(address, true);
		try {
			addr.setPersonal(personal, "UTF-8");
		} catch (UnsupportedEncodingException e) {
		}
		return addr;
	}
	
}//end class SMTP
