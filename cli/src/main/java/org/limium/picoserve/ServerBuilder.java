package org.limium.picoserve;

import java.io.IOException;
import java.net.InetSocketAddress;
import java.util.LinkedList;
import java.util.List;
import java.util.concurrent.Executor;

public class ServerBuilder {
    private InetSocketAddress mAddress = new InetSocketAddress(9000);
    private int backlog = 5;
    private List<Handler> handlers = new LinkedList<Handler>();
    private Executor executor = null;

    public ServerBuilder port(final int port) {
        mAddress = new InetSocketAddress(port);
        return this;
    }

    public ServerBuilder backlog(final int backlog) {
        this.backlog = backlog;
        return this;
    }

    public ServerBuilder address(final InetSocketAddress addr) {
        mAddress = addr;
        return this;
    }

    public ServerBuilder handle(final Handler handler) {
        handlers.add(handler);
        return this;
    }

    public ServerBuilder GET(final String path, final Server.Processor processor) {
        handlers.add(new Handler(path, "GET", request -> processor.process(request)));
        return this;
    }

    public ServerBuilder POST(final String path, final Server.Processor processor) {
        handlers.add(new Handler(path, "POST", request -> processor.process(request)));
        return this;
    }

    public ServerBuilder PUT(final String path, final Server.Processor processor) {
        handlers.add(new Handler(path, "PUT", request -> processor.process(request)));
        return this;
    }

    public ServerBuilder DELETE(final String path, final Server.Processor processor) {
        handlers.add(new Handler(path, "DELETE", request -> processor.process(request)));
        return this;
    }

    public ServerBuilder HEAD(final String path, final Server.Processor processor) {
        handlers.add(new Handler(path, "HEAD", request -> processor.process(request)));
        return this;
    }

    public ServerBuilder executor(final Executor executor) {
        this.executor = executor;
        return this;
    }

    public Server build() throws IOException {
        return new Server(mAddress, backlog, handlers, executor);
    }
}
