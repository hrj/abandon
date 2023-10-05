package org.limium.picoserve;

public class Handler {
    public final String path;
    public final Server.Processor processor;
    public final String[] methods;

    public Handler(final String path, final Server.Processor processor) {
        this.path = path;
        this.processor = processor;
        this.methods = new String[]{};
    }

    public Handler(final String path, final String methods, final Server.Processor processor) {
        this.path = path;
        this.processor = processor;
        this.methods = methods.split(",");
    }
}
