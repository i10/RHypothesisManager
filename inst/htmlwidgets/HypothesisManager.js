HTMLWidgets.widget({
    name: 'HypothesisManager',
    type: 'output',

    factory: function (el, width, height) {
        // TODO: define shared variables for this instance
        const container = d3.select(el);

        const menu = container.select("#menu");
        const svg = container.select("svg");
        const copy = container.select("#copy"),
            edit = container.select("#edit");
        const selector = container.select("#selector > ul");
        const legend = container.select("#legend > ul");
        const tooltip = container.select("#tooltip")
            .on("mouseleave", function () {
                tooltip.style("display", "none");
            });

        const tooltip_content = tooltip.select("div");

        return {
            renderValue: function (x) {
                svg.selectAll("*").remove();
                selector.selectAll("*").remove();
                legend.selectAll("*").remove();

                copy.property("disabled", true);
                edit.property("disabled", true);
                tooltip.attr("style", null);
                tooltip_content.selectAll("*").remove();

                const variables = x.variables,
                      functions = x.functions,
                      hypotheses = x.hypotheses;

                const color = d3.scaleOrdinal(d3.schemeCategory10);

                color.domain(d3.map(hypotheses, function(d) { return d.id; }).keys());

                const streams = functions.reduce(function (acc, func, i, arr) {
                    // Scope reduction
                    const arg_functions = functions.filter(function(f, ii) { return ii < i; }).filter(function (f) { return func.arguments.indexOf(f.id) !== -1; }),
                          arg_variables = variables.filter(function (v) { return func.arguments.indexOf(v.id) !== -1; }),
                          arg_hypotheses = hypotheses.filter(function (h) { return func.arguments.indexOf(h.id) !== -1; });

                    // Replacement of the arguments in a list with the appropriate entities
                    func.arguments = func.arguments.map(function (arg) {
                        const f = arg_functions.reduce(function (acc, f) { return f.id === arg ? f : acc; }, null),
                              v = arg_variables.reduce(function (acc, v) { return v.id === arg ? v : acc; }, null),
                              h = arg_hypotheses.reduce(function (acc, h) { return h.id === arg ? h : acc; }, null);

                        if (f !== null)
                            return f;

                        if (v !== null)
                            return v;

                        if (h !== null)
                            return h;

                        return arg;
                    });

                    const own_categories = hypotheses
                              .filter(function (h) { return h.functions.indexOf(func.id) !== -1 ||
                                                            arg_variables.reduce(function(acc, v) { return acc || (h.models.indexOf(v.id) !== -1); }, false); }),
                          child_categories = arg_functions
                              .reduce(function (acc, f) { return acc.concat(f.categories); }, []);

                    func.categories = own_categories.concat(child_categories);

                    // Since the nested function won't be showing up on the top level,
                    //     we can interrupt here and use the accumulated stuff further on
                    //     when the top-level function is invoked
                    if (func.depth !== 1) {
                        return acc;
                    }

                    function argument_recursion (acc, arg, i, arr) {
                        if ((arg instanceof Object) && !(arg instanceof Array) && arg.hasOwnProperty("id") && arg.id[0] === "f") {
                            return arg.arguments.reduce(argument_recursion, acc);
                        }

                        acc.push(arg);

                        return acc;
                    }

                    const products = variables.filter(function (v) { return v.origin === func.id; });

                    const args = func.arguments
                        .reduce(argument_recursion, [])
                        .filter(function (arg) { return (arg instanceof Object) && !(arg instanceof Array) && arg.hasOwnProperty("id") && arg.id[0] === "v"; })
                        .concat(products);

                    const vars = args
                        .reduce(function (acc, arg, i, arr) {
                            while (arg.generation > 0) {
                                if (arg.precursors.length) {
                                    const precursors = variables.filter(function (v) { return arg.precursors.indexOf(v.id) !== -1; });

                                    const data_precursors = precursors.filter(function (v) { return v.type === "data" || v.type === "model"; });

                                    if (arg.type === "data" && data_precursors.length === 0) {
                                        // Assume that the data variable was constructed from the constants
                                        //   and reset it's 'generation' attribute as if it was a regular data variables
                                        arg.generation = 0;

                                    } else if (arg.type === "data") {
                                        acc.push(arg);

                                        arg = data_precursors[0];

                                    } else {
                                        acc.push(arg);

                                        arg = precursors[0];
                                    }

                                } else if (arg.type === "column") {
                                    acc.push(arg);

                                    var tmp = variables.reduce(function (acc, v) { return v.columns.indexOf(arg.id) !== -1 ? v : acc; }, null);

                                    if (tmp !== null)
                                        arg = tmp;

                                } else {
                                    break;
                                }
                            }

                            acc.push(arg);

                            return acc;
                        }, [])
                        .filter(function (arg) { return arg.type === "data"; })
                        .reduce(function (acc, arg, i, arr) {
                            if (acc.indexOf(arg) === -1)  // indexOf may not work properly, but == should
                                acc.push(arg);

                            return acc;
                        }, []);

                    func.breakpoint = vars.reduce(function (acc, arg) { return acc || func.breakpoint === arg.id }, false);

                    func.markers = args.filter(function(arg, i, arr) { return arg.type === "model"; });

                    // Iff there are models among the function products -- replace the markers with only those
                    const model_products = products.filter(function(arg, i, arr) { return arg.type === "model"; });

                    if (model_products.length)
                        func.markers = model_products;

                    // Stream-work
                    vars.filter(function (arg, i, arr) { return arg.generation === 0; })
                        .forEach(function (arg, i, arr) {
                            var stream = acc.reduce(function (acc, stream) { return stream.id === arg.id ? stream : acc; }, null);

                            // Cloning because stream overlaps
                            const notch = JSON.parse(JSON.stringify(func));
                            notch.parent = null;

                            if (stream === null) {
                                stream = {
                                    name: arg.name,
                                    id: arg.id,
                                    functions: []
                                };

                                acc.push(stream);

                            } else {
                                notch.parent = stream.functions
                                    .filter(function(old_notch, i, arr) {
                                        if (i === 0)
                                            return true;

                                        if (old_notch.breakpoint)
                                            return true;

                                        const categories_ids =      notch.categories.map(function (arg) { return arg.id; }),
                                              old_categories_ids =  old_notch.categories.map(function (arg) { return arg.id; }),
                                              marker_ids =          notch.markers.map(function (arg) { return arg.id; }),
                                              old_marker_ids =      old_notch.markers.map(function (arg) { return arg.id; });

                                        if (!categories_ids.length && !old_categories_ids.length && !marker_ids.length && !old_marker_ids.length)
                                            return true;

                                        else if (marker_ids.reduce(function (acc, arg, i, arr) { return acc || (old_marker_ids.indexOf(arg) !== -1) }, false))
                                            return true;

                                        else if (categories_ids.reduce(function (acc, arg, i, arr) { return acc || (old_categories_ids.indexOf(arg) !== -1) }, false))
                                            return true;

                                        return false;
                                    })
                                    .reverse()
                                    [0].id;
                            }

                            stream.functions.push(notch);
                        });

                        return acc;
                    }, [])
                    .sort(function (a, b) {
                        return b.functions.length - a.functions.length;  // Descending order
                    });

                selector.selectAll("li")
                    .data(streams).enter()
                    .append("li")
                    .text(function (d) { return d.name + ":" + d.functions.length; })
                    .attr("title", function (d) { return d.name; })
                    .on("click", draw);

                function draw (stream_, i, arr) {
                    svg.selectAll(".stream").remove();

                    const g = svg.append("g")
                        .attr("class", "stream");

                    var stream = d3.stratify()
                        .id(function (d) { return d.id; })
                        .parentId(function (d) { return d.parent; })
                        (stream_.functions);

                    var nodes = d3.hierarchy(stream);

                    const depth = nodes.height;

                    function breadth_seek(acc, node) {
                        if (node.hasOwnProperty("children")) {
                            node.children.forEach(function (node) {
                                acc = breadth_seek(acc, node);
                            });
                        }

                        acc[node.depth] = acc[node.depth] ? acc[node.depth] + 1 : 1;

                        return acc;
                    }

                    const breadth = Math.max.apply(null, breadth_seek(Array(depth), nodes));

                    const tree = d3.tree().size([breadth * 120, depth * 30]);

                    // maps the node data to the tree layout
                    nodes = tree(nodes);

                    g.append("text")
                        .attrs({
                            x: nodes.x,
                            y: -20,
                            dy: ".35em"
                        })
                        .style("text-anchor", "middle")
                        .text(stream_.name);

                    var link = g.selectAll(".link")
                        .data(nodes.descendants().slice(1))
                        .enter().append("path")
                        .attrs({
                            class: "link",
                            d: function (d) {
                                return "M" + d.x + "," + d.y
                                    + "C" + d.x + "," + (d.y + d.parent.y) / 2
                                    + " " + d.parent.x + "," + (d.y + d.parent.y) / 2
                                    + " " + d.parent.x + "," + d.parent.y;
                            }
                        });

                    // adds each node as a group
                    var node = g.selectAll(".node")
                        .data(nodes.descendants())
                        .enter().append("g")
                        .attr("class", function (d) {
                            const func = d.data.data;

                            return [
                                "node",
                                (d.children ? "node--internal" : "node--leaf"),
                                (func.breakpoint ? "breakpoint" : ""),
                                (func.marker ? "marker" : "")
                            ].join(" ").trim();
                        })
                        .attr("transform", function (d) {
                            return "translate(" + d.x + "," + d.y + ")";
                        })
                        .on("mouseover", function (d) {
                            const func = d.data.data;

                            const bbox = this.getBoundingClientRect();

                            tooltip
                                .styles({
                                    "display": "block",
                                    "left": (bbox.right - 4) + "px",
                                    "top": (bbox.top - 9) + "px"
                                });

                            tooltip_content.selectAll("*").remove();

                            const comment_re = /#+ *(.+)$/ig;

                            const comment_start = func.signature.search(comment_re);

                            if (comment_start !== -1) {
                                const comment = comment_re.exec(func.signature)[1];

                                // TODO: reformat the signature
                                tooltip_content.append("p")
                                    .text(func.signature.substr(0, comment_start).trim());

                                tooltip_content.append("p")
                                    .attr("class", "comment")
                                    .html("<span>Tail comment:</span> " + comment);

                            } else {
                                tooltip_content.append("p")
                                    .text(func.signature);
                            }

                            if (!!func.packages) {
                                tooltip_content.append("hr");

                                const help = tooltip_content.append("p")
                                    .text("From packages: ");

                                func.packages.forEach(function (pkg, i, arr) {
                                    help.append("a")
                                        .attrs({
                                            href: pkg,
                                            class: "help"
                                        })
                                        .on("click", function () {
                                            Shiny.setInputValue("help", [pkg, func.name]);
                                        })
                                        .text(pkg);

                                    if (i !== arr.length - 1)
                                        help.append(",")
                                });
                            }
                        })
                        .on("mouseout", function (d) {
                            const x = d3.event.pageX;
                            const y = d3.event.pageY;

                            const bbox = tooltip.node().getBoundingClientRect();

                            // if (!(x >= bbox.left - 10 && x <= bbox.right + 10 && y >= bbox.top - 10 && y <= bbox.bottom + 10)) {
                            if (!(x >= bbox.left && x <= bbox.right && y >= bbox.top && y <= bbox.bottom)) {
                                tooltip.style("display", "none");
                            }
                        });

                    // adds the circle to the node
                    node.append("g")
                        .attr("class", "circle")
                        .selectAll("path.slice")
                        .data(function (d) {
                            var category_data = [null];

                            if (d.data.data.categories.length)
                                category_data = d.data.data.categories.map(function (cat, i, arr) { return cat.id; })

                            return d3.pie().value(function (d) { return 1; })(category_data);
                        })
                        .enter()
                        .append("path")
                        .attrs({
                            d: d3.arc().innerRadius(0).outerRadius(5),
                            class: "slice"
                        })
                        .style("fill",   function (d) { return d.data ? color(d.data) : null; })
                        .style("stroke", function (d) { return d.data ? color(d.data) : null; });

                    node.append("text")
                        .attrs({
                            dy: ".35em",
                            x: 10
                        })
                        .style("text-anchor", "start")
                        .text(function (d) {
                            const func = d.data.data;
                            return func.name;
                        });

                    node.append("text")
                        .attrs({
                            dy: ".35em",
                            x: -10
                        })
                        .style("text-anchor", "end")
                        .text(function (d) {
                            const func = d.data.data;
                            return func.markers.reduce(function (acc, arg, i, arr) { return acc && acc !== arg.name ? "…" : arg.name; }, null);
                        });

                    node.each(function (d, i, arr) {
                        const bbox = this.getBBox();

                        d3.select(this).insert("rect", ".circle")
                            .attrs({
                                x: bbox.x - 5,
                                y: bbox.y - 2,
                                width: bbox.width + 10,
                                height: bbox.height + 4,
                                rx: 5,
                                ry: 5
                            })
                    });

                    const bbox = g.node().getBoundingClientRect();

                    const offsetLeft = -(bbox.left + svg.node().parentNode.scrollLeft - svg.node().offsetLeft - 25);

                    g.attr("transform", "translate(" + offsetLeft + ", 50)");

                    svg.attrs({
                        width: bbox.width + 50,
                        height: bbox.height + 50
                    });

                    svg.insert("rect", "g")
                        .attrs({
                            id: "canvas",
                            x: 0,
                            y: 0,
                            width: "100%",
                            height: "100%"
                        });

                    const stream_categories = stream_.functions
                        .reduce(function (acc, func, i, arr) {
                            return acc.concat(func.categories);
                        }, [])
                        .map(function (e) { return e.id })
                        .filter(function (e, i, arr) {
                            return arr.indexOf(e) === i;
                        });

                    legend.selectAll("*").remove();

                    const categories = legend.selectAll("li")
                        .data(hypotheses)
                        .enter().append("li")
                        .classed("legend", true)
                        .classed("useless", function (d) { return stream_categories.indexOf(d.id) === -1 })
                        .attr("title", function (d) { return d.name; })
                        .on("click", function (d) {
                            Shiny.setInputValue("edit_hypothesis", d.id);
                        })
                        .text(function (d) { return d.name; });

                    categories.append("span")
                        .style("background-color", function(d) { return color(d.id); });

                    selector.selectAll("li")
                        .attr("class", null)
                        .filter(function (d, ii) { return ii === i;})
                        .attr("class", "chosen");

                    const selection_rectangle = {};

                    const drag = d3.drag()
                        .on("start", function () {
                            const event = d3.event.sourceEvent;

                            event.preventDefault();

                            selection_rectangle.anchor = {x: event.offsetX, y: event.offsetY};
                            selection_rectangle.node = svg.append("rect")
                                .attrs({
                                    id: "select",
                                    x: event.offsetX,
                                    y: event.offsetY
                                });

                            g.style("pointer-events", "none");
                        })
                        .on("drag", function () {
                            const event = d3.event.sourceEvent;

                            const top = Math.min(selection_rectangle.anchor.y, event.offsetY),
                                left = Math.min(selection_rectangle.anchor.x, event.offsetX),
                                bottom = Math.max(selection_rectangle.anchor.y, event.offsetY),
                                right = Math.max(selection_rectangle.anchor.x, event.offsetX);

                            if (event.target.parentNode !== svg.node() && event.target !== svg.node())
                                return;

                            selection_rectangle.node.attrs({
                                x: left,
                                y: top,
                                width: right - left,
                                height: bottom - top
                            });

                            const direction = {
                                x: event.offsetX - selection_rectangle.anchor.x,
                                y: event.offsetY - selection_rectangle.anchor.y
                            };
                            direction.distance = Math.sqrt(Math.pow(direction.x, 2) + Math.pow(direction.y, 2));
                            direction.axis = Math.abs(direction.x) >= Math.abs(direction.y) ? "x" : "y";
                            direction.x /= Math.abs(direction.x);
                            direction.y /= Math.abs(direction.y);

                            var closest_node_distance = Infinity;
                            var closest_node;

                            node.each(function (d) {
                                const $this = d3.select(this);

                                const client_rect = $this.select(".circle").node().getBoundingClientRect(),
                                    viewport_client_rect = this.viewportElement.getBoundingClientRect();

                                const node_direction = {
                                    x: client_rect.left - viewport_client_rect.left + client_rect.width / 2 - selection_rectangle.anchor.x,
                                    y: client_rect.top - viewport_client_rect.top + client_rect.height / 2 - selection_rectangle.anchor.y
                                };

                                node_direction.axis = Math.abs(node_direction.x) > Math.abs(node_direction.y) ? "x" : "y";
                                node_direction.distance = Math.sqrt(Math.pow(node_direction.x, 2) + Math.pow(node_direction.y, 2));
                                node_direction.x /= Math.abs(node_direction.x);
                                node_direction.y /= Math.abs(node_direction.y);

                                if ((direction.axis === node_direction.axis) &&
                                    (direction[direction.axis] === node_direction[node_direction.axis]) &&
                                    node_direction.distance < closest_node_distance) {
                                    closest_node_distance = node_direction.distance;
                                    closest_node = d;
                                }
                            });

                            if (!closest_node)
                                return;

                            const available_nodes = [closest_node];
                            var parent = closest_node;

                            var restricted_categories = closest_node.data.data.categories.map(function (cat) { return cat.id; });

                            if (!restricted_categories.length && node.filter(".selected").size()) {
                                node.filter(".selected").each(function (d) {
                                    restricted_categories = restricted_categories.concat(d.data.data.categories.map(function (cat) { return cat.id; }));
                                });

                                restricted_categories = restricted_categories.filter(function (h, i, arr) { return arr.indexOf(h) === i; })
                            }

                            while (!!parent.parent && !parent.data.data.breakpoint && (!parent.data.data.categories.length || !restricted_categories.length || parent.data.data.categories.map(function (cat) { return cat.id; }).reduce(function(acc, cat) { return acc || restricted_categories.indexOf(cat) !== -1; }, false))) {
                                parent = parent.parent;
                                available_nodes.push(parent);
                            }

                            function rec(acc, child, i, arr) {
                                if (child.data.data.breakpoint ||
                                    child.data.data.categories.length && restricted_categories.length && !child.data.data.categories.map(function (cat) { return cat.id; }).reduce(function(acc, cat) { return acc || restricted_categories.indexOf(cat) !== -1; }, false)) {
                                    return acc;
                                }

                                available_nodes.push(child);

                                if (child.height === 0) {
                                    acc.push(child);
                                    return acc;
                                }

                                return acc.concat(child.children.reduce(rec, []));
                            }

                            const children = parent.children.reduce(rec, []);

                            node
                                .classed("unreachable", function (d) { return available_nodes.indexOf(d) === -1 })
                                .classed("selected", function (d) {
                                    if (available_nodes.indexOf(d) === -1)
                                        return false;

                                    const client_rect = this.getBoundingClientRect(),
                                        viewport_client_rect = this.viewportElement.getBoundingClientRect();

                                    const node_top = client_rect.top - viewport_client_rect.top,
                                        node_left = client_rect.left - viewport_client_rect.left,
                                        node_bottom = client_rect.bottom - viewport_client_rect.top,
                                        node_right = client_rect.right - viewport_client_rect.left;

                                    return node_left < right && node_right > left && node_top < bottom && node_bottom > top;
                                });
                        })
                        .on("end", function () {
                            selection_rectangle.node.remove();
                            g.style("pointer-events", null);

                            const selected_functions = [];
                            var selected_hypotheses = [];

                            node.filter(".unreachable").classed("unreachable", false);

                            node.filter(".selected").each(function (d) {
                                selected_functions.push(d.data.id);
                                selected_hypotheses = selected_hypotheses.concat(d.data.data.categories);
                            });

                            if (selected_functions.length) {
                                Shiny.setInputValue("select", selected_functions)
                            }

                            const selected_hypotheses_ids = selected_hypotheses
                                .map(function (h) { return h.id; })
                                .filter(function (h, i, arr) { return arr.indexOf(h) === i; });

                            if (selected_hypotheses_ids.length === 1) {
                                copy.property("disabled", false);
                                edit.property("disabled", false);
                                Shiny.setInputValue("copy_hypothesis", selected_hypotheses_ids[0]);

                            } else {
                                copy.property("disabled", true);
                                edit.property("disabled", true);
                                Shiny.setInputValue("copy_hypothesis", null);
                            }
                        });

                    svg.call(drag);
                }

                if (streams.length) {
                    draw(streams[0], 0, streams);
                }
            },

            resize: function (width, height) {
                // TODO: code to re-render the widget with a new size
            }
        };
    }
});