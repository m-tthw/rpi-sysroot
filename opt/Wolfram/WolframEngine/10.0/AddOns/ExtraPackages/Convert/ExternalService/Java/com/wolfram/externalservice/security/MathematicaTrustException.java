package com.wolfram.externalservice.security;

import java.security.GeneralSecurityException;

public class MathematicaTrustException extends GeneralSecurityException {

	/**
	 * 
	 */
	private static final long serialVersionUID = -3905901028936810875L;
	
	public MathematicaTrustException(String reason) {
		super(reason);
	}
	
	public MathematicaTrustException(String message, Throwable cause) {
		super(message);
		initCause(cause);
	}
	
	public MathematicaTrustException(Throwable cause) {
		super(cause == null ? null : cause.toString());
		initCause(cause);
	}
	
}