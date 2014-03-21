package System.Convert;
import javax.media.*;
import javax.media.control.FramePositioningControl;

import javax.media.control.FrameGrabbingControl;
import javax.media.control.TrackControl;
import javax.media.util.BufferToImage;
import javax.media.format.*;
import java.awt.image.Raster;
import java.awt.image.BufferedImage;

public class MovieDump {

	private Player p;

	private Processor processor;

	private FramePositioningControl fpc;

	private FrameGrabbingControl fgc;

	private MediaLocator ml;

	private Buffer frame;

	private Format format;

	private RGBFormat rgbformat = null;

	private BufferToImage bti;

	private BufferedImage biFrame;

	private int w, h, c, minx, miny;

	private int[] pix = null;

	private Raster data = null;

	private int framecount;

	private double framerate;

	private String encoding = "";

	private void closeProcessor() {
		if (processor != null) {
			processor.deallocate();
			processor.close();
			processor = null;
		}

	}

	public boolean getMovieInfo(String s) {
		ml = new MediaLocator("file:" + s);

		try {
			processor = Manager.createProcessor(ml);
		} catch (Exception e) {
			//System.out.println("Error opening file: " + s);
			ml = null;
			return false;
		}
		ml = null;
		if (processor == null) {
			//System.out.println("Error creating processor.");
			return false;
		}

		processor.realize();

		while (processor.getState() != processor.Realized) {
			if (processor.getState() == processor.Unrealized) {
				//System.out.println("Error realizing processor.");
				closeProcessor();
				return false;
			}
			try {
				Thread.sleep(50);
			} catch (InterruptedException e) {
			}
		}
		TrackControl[] trackcontrols = null;
		try {
			trackcontrols = processor.getTrackControls();
		} catch (Error e) {
			//System.out.println("Error getting tracks from processor.");
			closeProcessor();
			return false;
		}

		if (trackcontrols == null) {
			//System.out.println("Error getting track controls from processor.");
			closeProcessor();
			return false;
		}

		Format video = null;
		for (int i = 0; i < trackcontrols.length
				&& !(video instanceof VideoFormat); i++) {
			video = trackcontrols[i].getFormat();
		}

		encoding = video.getEncoding();
		w = (int) (.5 + (((VideoFormat) video).getSize().getWidth()));
		h = (int) (.5 + (((VideoFormat) video).getSize().getHeight()));
		framerate = ((VideoFormat) video).getFrameRate();
		framecount = (int) (.5 + processor.getDuration().getSeconds()
				* framerate);

		video = null;
		closeProcessor();

		return true;
	}

	public String getEncoding() {
		return encoding;
	}

	public int getFrameCount() {
		return framecount;
	}

	public double getFrameRate() {
		return framerate;
	}

	public void close() {

		fpc = null;
		fgc = null;
		ml = null;
		data = null;
		pix = null;
		frame = null;
		format = null;
		bti = null;
		biFrame = null;

		if (p != null) {
			p.stop();
			p.deallocate();
			while (p.getState() > p.Realized) {
				try {
					Thread.sleep(50);
				} catch (InterruptedException e) {
				}
			}
			p.close();
			/*try {
				Thread.sleep(100);
			} catch (Exception e) {}
			*/
			p = null;
		}

	}

	public boolean openFile(String s) {

		try {

			ml = new MediaLocator("file:" + s);
			//System.out.println(ml.toString());
			try {
				p = Manager.createRealizedPlayer(ml);
			} catch (Exception e) {
				//System.out.println("Error creating Player!");
				//System.out.println(e.toString());
				p = null;
				return false;
			}

			p.prefetch();
			while (p.getState() != p.Prefetched) {
				if (p.getState() == p.Unrealized) {
					//System.out.println("Error prefetching player.");
					return false;
				}
				try {
					Thread.sleep(50);
				} catch (InterruptedException e) {
				}
			}
		
			
			fpc = (FramePositioningControl) p
					.getControl("javax.media.control.FramePositioningControl");
			if (fpc == null) {
				//System.out.println("Error getting FramePositioningControl");
				return false;
			}
			//System.out.println(fpc.toString());
			Time duration = p.getDuration();

			if (duration == Duration.DURATION_UNKNOWN) {
				//System.out.println("Unknown Duration");
				return false;
			}

			framecount = fpc.mapTimeToFrame(duration);
			if (framecount == FramePositioningControl.FRAME_UNKNOWN) {
				//System.out.println("Format does not support mapTimeToFrame");
				return false;
			}

			framecount += 1; //Shift to 1 indexed frames

			framerate = framecount / duration.getSeconds();

			fgc = (FrameGrabbingControl) p
					.getControl("javax.media.control.FrameGrabbingControl");
			if (fgc == null) {
				//System.out.println("Error getting FrameGrabbingControl");
				return false;
			}

			//	System.out.println(fgc.toString());
			if (!seek(1)) {
				//System.out.println("Error Seeking");
				return false;
			}

			frame = fgc.grabFrame();
			if (frame == null) {
				//System.out.println("Error grabbing Frame");
				return false;
			}
		

			bti = new BufferToImage((VideoFormat) frame.getFormat());
			if (bti == null) {
				//System.out.println("Error creating BufferToImage");
				return false;
			}

			biFrame = (BufferedImage) bti.createImage(frame);
			if (biFrame == null) {
				//System.out.println("Error creating BufferedImage from Buffer");
				return false;
			}
			//System.out.println("biFrame " + biFrame);

			data = biFrame.getData();
			if (data == null) {
				//System.out.println("Error getting data from BufferedImage");
				return false;
			}

			w = data.getWidth();
			h = data.getHeight();
			c = data.getNumBands();
			pix = new int[c * w * h];

			//System.out.println("BTI " + bti);

			frame = null;
			ml = null;

		} catch (Exception e) {
			//System.out.println(e.toString());
			return false;

		}
		return true;

	}

	public int getWidth() {
		return w;
	}

	public int getHeight() {
		return h;
	}

	public int getChannels() {
		return c;
	}

	public boolean seek(int i) {
		//System.out.println("Seeking " + i);
		return fpc.seek(i - 1) == i - 1;
	}

	public int[] grabFrame(int frameNum) {
		if (!seek(frameNum)) {
			//System.out.println("Error seeking frame " + frameNum);
			return null;
		}

		//System.out.println("Grabbing");
		frame = fgc.grabFrame();
		if (frame == null) {
			//System.out.println("Error grabbing frame");
			return null;
		}



		//System.out.println("Frame: " + frame);
		biFrame = (BufferedImage) bti.createImage(frame);

		if (biFrame == null) {
			//System.out.println("Error creating BufferedImage from Buffer");
			return null;
		}

		//System.out.println("biFrame " + biFrame);

		data = biFrame.getData();
		if (data == null) {
			//System.out.println("Error getting data from BufferedImage.");
			return null;
		}

		// Set by openFile
		//w = data.getWidth();
		//h = data.getHeight();
		//c = data.getNumBands();
		//pix = new int[c*w*h];

		minx = data.getMinX();
		miny = data.getMinY();
		return data.getPixels(minx, miny, w, h, pix);

		//return biFrame;

	}

}
