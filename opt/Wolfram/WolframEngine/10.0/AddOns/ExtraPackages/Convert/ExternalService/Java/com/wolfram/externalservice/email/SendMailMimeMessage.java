package com.wolfram.externalservice.email;

import javax.mail.internet.MimeMessage;
import javax.mail.MessagingException;
import javax.mail.Session;

public class SendMailMimeMessage extends MimeMessage
{
	private String messageID;
	
	public  SendMailMimeMessage(Session session, String messageID)
	throws MessagingException {
		super(session);
		this.messageID = messageID;
	}
	
	@Override
	protected void updateMessageID() throws MessagingException {
		setHeader("Message-ID", this.messageID);
	    }
	
}