use std::convert::From;
use std::rc::Rc;
use virtual_dom_rs::prelude::*;
use virtual_dom_rs::{Events, IterableNodes, VElement, VText};

pub struct InjectNode(pub Rc<VirtualNode>);

impl From<InjectNode> for IterableNodes {
    fn from(h: InjectNode) -> IterableNodes {
        fn clone(n: &VirtualNode) -> VirtualNode {
            match n {
                VirtualNode::Element(e) => VirtualNode::Element(VElement {
                    tag: e.tag.clone(),
                    attrs: e.attrs.clone(),
                    events: Events(e.events.0.clone()),
                    children: e.children.iter().map(|v| clone(v)).collect(),
                }),
                VirtualNode::Text(txt) => VirtualNode::Text(VText {
                    text: txt.to_string(),
                }),
            }
        }

        clone(&*h.0).into()
    }
}
