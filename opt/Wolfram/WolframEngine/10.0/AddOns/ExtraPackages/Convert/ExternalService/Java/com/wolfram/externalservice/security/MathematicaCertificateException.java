package com.wolfram.externalservice.security;

import java.security.cert.CertificateException;

public class MathematicaCertificateException extends CertificateException {

	/**
	 * 
	 */
	private static final long serialVersionUID = -3905901028936810875L;
	
	public MathematicaCertificateException(String reason) {
		super(reason);
	}
	
	public MathematicaCertificateException(String message, Throwable cause) {
		super(message);
		initCause(cause);
	}
	
	public MathematicaCertificateException(Throwable cause) {
		super(cause == null ? null : cause.toString());
		initCause(cause);
	}
	
}