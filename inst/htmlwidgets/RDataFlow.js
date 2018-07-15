HTMLWidgets.widget({
    name: 'RDataFlow',
    type: 'output',

    factory: function (el, width, height) {
        // TODO: define shared variables for this instance

        return {
            renderValue: function (x) {
                const nodes = x.variables,
                    links = [];

                nodes.forEach(function(node) {
                    node.precursors.forEach(function(precursor_id) {
                        links.push({
                            value: 10,
                            source: node.id,
                            target: precursor_id
                        })
                    })
                });

                const svg = d3.select("svg");

                svg.attr("width", width);
                svg.attr("height", height);

                svg.selectAll("*").remove();

                const color = d3.scaleOrdinal(d3.schemeCategory20);

                const simulation = d3.forceSimulation()
                    .force("link", d3.forceLink().id(function (d) {
                        return d.id;
                    }))
                    .force("charge", d3.forceManyBody())
                    .force("center", d3.forceCenter(width / 2, height / 2));

                const link = svg.append("g")
                    .attr("class", "links")
                    .selectAll("line")
                    .data(links)
                    .enter().append("line")
                    .attr("stroke-width", function (d) {
                        return Math.sqrt(d.value);
                    });

                const node = svg.append("g")
                    .attr("class", "nodes")
                    .selectAll("circle")
                    .data(nodes)
                    .enter().append("circle")
                    .attr("r", 5)
                    .attr("fill", function (d) {
                        return color(d.group);
                    })
                    .call(d3.drag()
                        .on("start", dragstarted)
                        .on("drag", dragged)
                        .on("end", dragended));

                node.append("title")
                    .text(function (d) { return d.name; });

                simulation
                    .nodes(nodes)
                    .on("tick", ticked);

                simulation.force("link")
                    .links(links);

                function ticked() {
                    link
                        .attr("x1", function (d) { return d.source.x; })
                        .attr("y1", function (d) { return d.source.y; })
                        .attr("x2", function (d) { return d.target.x; })
                        .attr("y2", function (d) { return d.target.y; });

                    node
                        .attr("cx", function (d) { return d.x; })
                        .attr("cy", function (d) { return d.y; });
                }

                function dragstarted(d) {
                    if (!d3.event.active) simulation.alphaTarget(0.3).restart();
                    d.fx = d.x;
                    d.fy = d.y;
                }

                function dragged(d) {
                    d.fx = d3.event.x;
                    d.fy = d3.event.y;
                }

                function dragended(d) {
                    if (!d3.event.active) simulation.alphaTarget(0);
                    d.fx = null;
                    d.fy = null;
                }
            },

            resize: function (width, height) {
                // TODO: code to re-render the widget with a new size
            }
        };
    }
});