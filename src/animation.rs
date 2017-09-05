
// Copyright 2017 The gltf Library Developers
//
// Licensed under the Apache License, Version 2.0 <LICENSE-APACHE or
// http://www.apache.org/licenses/LICENSE-2.0> or the MIT license
// <LICENSE-MIT or http://opensource.org/licenses/MIT>, at your
// option. This file may not be copied, modified, or distributed
// except according to those terms.

use {accessor, json, scene, Gltf};
use child_iter::{ChildIter, ChildBuilder};

pub use json::animation::{InterpolationAlgorithm, TrsProperty};

/// A keyframe animation.
#[derive(Clone, Debug)]
pub struct Animation<'a> {
    /// The parent `Gltf` struct.
    gltf: &'a Gltf,

    /// The corresponding JSON index.
    index: usize,

    /// The corresponding JSON struct.
    json: &'a json::animation::Animation,
}

impl<'a> Animation<'a> {
    /// Constructs an `Animation`.
    pub(crate) fn new(gltf: &'a Gltf, index: usize, json: &'a json::animation::Animation) -> Self {
        Self {
            gltf: gltf,
            index: index,
            json: json,
        }
    }

    /// Returns the internal JSON item.
    pub fn as_json(&self) -> &json::animation::Animation {
        self.json
    }

    /// Returns the internal JSON index.
    pub fn index(&self) -> usize {
        self.index
    }

    /// Optional application specific data.
    pub fn extras(&self) -> &json::Extras {
        &self.json.extras
    }

    /// Returns an `Iterator` over the animation channels.
    ///
    /// Each channel targets an animation's sampler at a node's property.
    pub fn channels(&self) -> ChildIter<'a, Channel<'a>> {
        ChildIter::new(self.clone(), self.json.channels.iter())
    }

    /// Optional user-defined name for this object.
    #[cfg(feature = "names")]
    pub fn name(&self) -> Option<&str> {
        self.json.name.as_ref().map(String::as_str)
    }

    /// Returns an `Iterator` over the animation samplers.
    ///
    /// Each sampler combines input and output accessors with an
    /// interpolation algorithm to define a keyframe graph (but not its target).
    pub fn samplers(&self) -> ChildIter<'a, Sampler<'a>> {
        ChildIter::new(self.clone(), self.json.samplers.iter())
    }
}

/// Targets an animation's sampler at a node's property.
#[derive(Clone, Debug)]
pub struct Channel<'a> {
    /// The parent `Animation` struct.
    anim: Animation<'a>,

    /// The corresponding JSON struct.
    json: &'a json::animation::Channel,
}

impl<'a> ChildBuilder<'a> for Channel<'a> {
    type Parent = Animation<'a>;
    type Json = json::animation::Channel;

    /// Constructs a `Channel`.
    fn new(anim: Animation<'a>, json: &'a json::animation::Channel) -> Self {
        Self {
            anim: anim,
            json: json,
        }
    }
}

impl<'a> Channel<'a> {
    /// Returns the parent `Animation` struct.
    pub fn animation(&self) -> Animation<'a> {
        self.anim.clone()
    }

    /// Returns the internal JSON item.
    pub fn as_json(&self) -> &json::animation::Channel {
        self.json
    }

    /// Returns the sampler in this animation used to compute the value for the
    /// target.
    pub fn sampler(&self) -> Sampler {
        self.anim.samplers().nth(self.json.sampler.value()).unwrap()
    }

    /// Returns the node and TRS property to target.
    pub fn target(&self) -> Target<'a> {
        Target::new(self.anim.clone(), &self.json.target)
    }

    /// Optional application specific data.
    pub fn extras(&self) -> &json::Extras {
        &self.json.extras
    }
}

/// The node and TRS property that an animation channel targets.
#[derive(Clone, Debug)]
pub struct Target<'a> {
    /// The parent `Animation` struct.
    anim: Animation<'a>,

    /// The corresponding JSON struct.
    json: &'a json::animation::Target,
}

impl<'a> Target<'a> {
    /// Constructs a `Target`.
    pub(crate) fn new(anim: Animation<'a>, json: &'a json::animation::Target) -> Self {
        Self {
            anim: anim,
            json: json,
        }
    }

    /// Returns the parent `Animation` struct.
    pub fn animation(&self) -> Animation<'a> {
        self.anim.clone()
    }

    /// Returns the internal JSON item.
    pub fn as_json(&self) -> &json::animation::Target {
        self.json
    }

    /// Optional application specific data.
    pub fn extras(&self) -> &json::Extras {
        &self.json.extras
    }

    /// Returns the target node.
    pub fn node(&self) -> scene::Node {
        self.anim.gltf.nodes().nth(self.json.node.value()).unwrap()
    }

    /// Returns the node's TRS property to modify or the 'weights' of the morph
    /// targets it instantiates.
    pub fn path(&self) -> TrsProperty {
        self.json.path.unwrap()
    }
}

/// Defines a keyframe graph (but not its target).
#[derive(Clone, Debug)]
pub struct Sampler<'a> {
    /// The parent `Animation` struct.
    anim: Animation<'a>,

    /// The corresponding JSON struct.
    json: &'a json::animation::Sampler,
}

impl<'a> ChildBuilder<'a> for Sampler<'a> {
    type Parent = Animation<'a>;
    type Json = json::animation::Sampler;

    /// Constructs a `Sampler`.
    fn new(anim: Animation<'a>, json: &'a json::animation::Sampler) -> Self {
        Self {
            anim: anim,
            json: json,
        }
    }
}

impl<'a> Sampler<'a> {
    /// Returns the parent `Animation` struct.
    pub fn animation(&self) -> Animation<'a> {
        self.anim.clone()
    }

    /// Returns the internal JSON item.
    pub fn as_json(&self) -> &json::animation::Sampler {
        self.json
    }

    /// Optional application specific data.
    pub fn extras(&self) -> &json::Extras {
        &self.json.extras
    }

    /// Returns the accessor containing the keyframe input values (e.g. time).
    pub fn input(&self) -> accessor::Accessor<'a> {
        self.anim
            .gltf
            .accessors()
            .nth(self.json.input.value())
            .unwrap()
    }

    /// Returns the keyframe interpolation algorithm.
    pub fn interpolation(&self) -> InterpolationAlgorithm {
        self.json.interpolation.unwrap()
    }

    /// Returns the accessor containing the keyframe output values.
    pub fn output(&self) -> accessor::Accessor<'a> {
        self.anim
            .gltf
            .accessors()
            .nth(self.json.output.value())
            .unwrap()
    }
}
