package com.wolfram.externalservice.security;

import java.io.IOException;

public class MathematicaSSLException extends IOException {

	private static final long serialVersionUID = -3905901028936810875L;
	
	public MathematicaSSLException(String reason) {
		super(reason);
	}
	
	public MathematicaSSLException(String message, Throwable cause) {
		super(message);
		initCause(cause);
	}
	
	public MathematicaSSLException(Throwable cause) {
		super(cause == null ? null : cause.toString());
		initCause(cause);
	}
	
}