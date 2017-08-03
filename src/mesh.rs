
// Copyright 2017 The gltf Library Developers
//
// Licensed under the Apache License, Version 2.0 <LICENSE-APACHE or
// http://www.apache.org/licenses/LICENSE-2.0> or the MIT license
// <LICENSE-MIT or http://opensource.org/licenses/MIT>, at your
// option. This file may not be copied, modified, or distributed
// except according to those terms.

use std::borrow::Cow;
use std::collections::hash_map;
use std::{iter, slice};
use {accessor, extensions, json, material};

use accessor::{Accessor, DataType, Dimensions, Iter};
use Gltf;

pub use json::mesh::{Mode, Semantic};

trait Normalized {
    type Denormalized;
    fn denormalize(&self) -> Self::Denormalized;
}

impl Normalized for u8 {
    type Denormalized = f32;
    fn denormalize(&self) -> Self::Denormalized {
        *self as f32 / 255.0
    }
}

impl Normalized for u16 {
    type Denormalized = f32;
    fn denormalize(&self) -> Self::Denormalized {
        *self as f32 / 65535.0
    }
}

impl<T: Copy + Normalized> Normalized for [T; 2] {
    type Denormalized = [T::Denormalized; 2];
    fn denormalize(&self) -> Self::Denormalized {
        [
            self[0].denormalize(),
            self[1].denormalize(),
        ]
    }
}

impl<T: Copy + Normalized> Normalized for [T; 3] {
    type Denormalized = [T::Denormalized; 3];
    fn denormalize(&self) -> Self::Denormalized {
        [
            self[0].denormalize(),
            self[1].denormalize(),
            self[2].denormalize(),
        ]
    }
}

impl<T: Copy + Normalized> Normalized for [T; 4] {
    type Denormalized = [T::Denormalized; 4];
    fn denormalize(&self) -> Self::Denormalized {
        [
            self[0].denormalize(),
            self[1].denormalize(),
            self[2].denormalize(),
            self[3].denormalize(),
        ]
    }
}

/// XYZ vertex normals of type `[f32; 3]`.
#[derive(Clone, Debug)]
pub struct Normals(Iter<[f32; 3]>);

/// XYZ vertex normal displacements of type `[f32; 3]`.
#[derive(Clone, Debug)]
pub struct NormalDisplacements(Iter<[f32; 3]>);

/// XYZ vertex positions of type `[f32; 3]`.
#[derive(Clone, Debug)]
pub struct Positions(Iter<[f32; 3]>);

/// XYZ vertex position displacements of type `[f32; 3]`.
#[derive(Clone, Debug)]
pub struct PositionDisplacements(Iter<[f32; 3]>);

/// XYZW vertex tangents of type `[f32; 4]` where the `w` component is a
/// sign value (-1 or +1) indicating the handedness of the tangent basis.
#[derive(Clone, Debug)]
pub struct Tangents(Iter<[f32; 4]>);

/// XYZ vertex tangent displacements of type `[f32; 3]`.
#[derive(Clone, Debug)]
pub struct TangentDisplacements(Iter<[f32; 3]>);

/// Vertex colors.
#[derive(Clone, Debug)]
pub enum Colors {
    /// RGB vertex color of type `[u8; 3]>`.
    RgbU8(Iter<[u8; 3]>),

    /// RGBA vertex color of type `[u8; 4]>`.
    RgbaU8(Iter<[u8; 4]>),

    /// RGB vertex color of type `[u16; 3]>`.
    RgbU16(Iter<[u16; 3]>),

    /// RGBA vertex color of type `[u16; 4]>`.
    RgbaU16(Iter<[u16; 4]>),

    /// RGB vertex color of type `[f32; 3]`.
    RgbF32(Iter<[f32; 3]>),

    /// RGBA vertex color of type `[f32; 4]`.
    RgbaF32(Iter<[f32; 4]>),
}

/// Index data.
#[derive(Clone, Debug)]
pub enum Indices {
    /// Index data of type U8
    U8(Iter<u8>),
    /// Index data of type U16
    U16(Iter<u16>),
    /// Index data of type U32
    U32(Iter<u32>),
}

/// Defines the vertex ordering of a `Primitive`.
#[derive(Clone, Debug)]
enum Order<'a> {
    /// Begin from 0, increment by one.
    Regular,

    /// Custom vertex order.
    Custom(Cow<'a, [u32]>),
}

/// An `Iterator` that traverses another iterator with the given `Order`.
#[derive(Clone, Debug)]
pub struct Ordered<'a, I> {
    /// The iterator ordering.
    order: Order<'a>,

    /// The index of the next iteration if the ordering is `Custom`.
    order_index: usize,

    /// The internal iterator to yield items from.
    iter: I,
}

impl<'a, I> Iterator for Ordered<'a, I>
    where I: Clone + Iterator
{
    type Item = I::Item;
    fn next(&mut self) -> Option<Self::Item> {
        match self.order {
            Order::Regular => self.iter.next(),
            Order::Custom(ref ordering) => {
                let order_index = self.order_index;
                self.order_index += 1;
                ordering
                    .get(order_index)
                    .map(|item_index| {
                        self.iter.clone().nth(*item_index as usize).unwrap()
                    })
            },
        }
    }
}

/// Intermediate type for data that is either provided or generated.
#[derive(Clone, Debug)]
pub enum Maybe<T> {
    /// The data was provided (and hence borrowed from an `Accessor`.)
    Provided(Iter<T>),

    /// The data was generated (and hence owned by the `Primitive`.)
    Generated(Vec<T>),
}

/// An `Iterator` that coerces index data to `u32`s.
#[derive(Clone, Debug)]
pub struct IndicesU32(Indices);

impl Iterator for IndicesU32 {
    type Item = u32;
    fn next(&mut self) -> Option<Self::Item> {
        match &mut self.0 {
            &mut Indices::U8(ref mut iter) => iter.next().map(|x| x as u32),
            &mut Indices::U16(ref mut iter) => iter.next().map(|x| x as u32),
            &mut Indices::U32(ref mut iter) => iter.next().map(|x| x),
        }
    }
}

/// Vertex joints.
/// Refer to the documentation on morph targets and skins for more
/// information.
#[derive(Clone, Debug)]
pub enum Joints {
    /// Joints of type `[u8; 4]`.
    /// Refer to the documentation on morph targets and skins for more
    /// information.
    U8(Iter<[u8; 4]>),
    
    /// Joints of type `[u16; 4]`.
    /// Refer to the documentation on morph targets and skins for more
    /// information.
    U16(Iter<[u16; 4]>),
}

/// UV texture co-ordinates.
#[derive(Clone, Debug)]
pub enum TexCoords {
    /// UV texture co-ordinates of type `[f32; 2]`.
    F32(Iter<[f32; 2]>),

    /// UV texture co-ordinates of type `[u8; 2]>`.
    U8(Iter<[u8; 2]>),

    /// UV texture co-ordinates of type `[u16; 2]>`.
    U16(Iter<[u16; 2]>),
}

/// An `Iterator` that coerces texture co-ordinates to `f32`s.
#[derive(Clone, Debug)]
pub struct TexCoordsF32(TexCoords);

impl Iterator for TexCoordsF32 {
    type Item = [f32; 2];
    fn next(&mut self) -> Option<Self::Item> {
        match &mut self.0 {
            &mut TexCoords::U8(ref mut i) => i.next().map(|x| x.denormalize()),
            &mut TexCoords::U16(ref mut i) => i.next().map(|x| x.denormalize()),
            &mut TexCoords::F32(ref mut i) => i.next().map(|x| x),
        }
    }
}

/// Weights,
/// Refer to the documentation on morph targets for more information.
#[derive(Clone, Debug)]
pub enum Weights {
    /// Weights of type `[f32; 4]`.
    F32(Iter<[f32; 4]>),

    /// Weights of type `[u8; 4]`.
    U8(Iter<[u8; 4]>),

    /// Weights of type `[u16; 4]`.
    U16(Iter<[u16; 4]>),
}

/// Vertex attribute data.
#[derive(Clone, Debug)]
pub enum Attribute {
    /// Vertex colors.
    Colors(u32, Colors),

    // TODO: Handle extras (needs to be handled elsewhere to avoid taking lifetime)
    // #[cfg(feature = "extras")]
    // Extras(&'a str, accessor::Accessor),

    /// Vertex joints.
    /// Refer to the documentation on morph targets and skins for more
    /// information.
    Joints(u32, Joints),

    /// XYZ vertex positions of type `[f32; 3]`.
    Positions(Positions),

    /// XYZ vertex normals of type `[f32; 3]`.
    Normals(Normals),

    /// XYZW vertex tangents of type `[f32; 4]` where the `w` component is a
    /// sign value (-1 or +1) indicating the handedness of the tangent basis.
    Tangents(Tangents),

    /// UV texture co-ordinates.
    TexCoords(u32, TexCoords),

    /// Weights.
    /// Refer to the documentation on morph targets for more information.
    Weights(u32, Weights),
}

/// Morph targets.
#[derive(Clone, Debug)]
pub struct MorphTarget {
    /// XYZ vertex position displacements.
    positions: Option<PositionDisplacements>,

    /// XYZ vertex normal displacements.
    normals: Option<NormalDisplacements>,

    /// XYZ vertex tangent displacements.
    tangents: Option<TangentDisplacements>,
}

/// A set of primitives to be rendered.  A node can contain one or more meshes and
/// its transform places the meshes in the scene.
#[derive(Clone, Debug)]
pub struct Mesh<'a>  {
    /// The parent `Gltf` struct.
    gltf: &'a Gltf,

    /// The corresponding JSON index.
    index: usize,

    /// The corresponding JSON struct.
    json: &'a json::mesh::Mesh,
}

/// An `Iterator` that visits the attributes of a `Primitive`.
#[derive(Clone, Debug)]
pub struct Attributes<'a> {
    /// The parent `Primitive` struct.
    prim: &'a Primitive<'a>,

    /// The internal attribute iter.
    iter: hash_map::Iter<'a, json::mesh::Semantic, json::Index<json::accessor::Accessor>>,
}

/// An `Iterator` that visits the primitives of a `Mesh`.
#[derive(Clone, Debug)]
pub struct Primitives<'a>  {
    /// The parent `Mesh` struct.
    mesh: &'a Mesh<'a>,

    /// The internal JSON primitive iterator.
    iter: iter::Enumerate<slice::Iter<'a, json::mesh::Primitive>>,
}

/// Fundamental positioning properties.
#[derive(Clone, Debug)]
pub struct Positioning<'a> {
    /// XYZ vertex positions.
    pub positions: Ordered<'a, Positions>,

    /// XYZ vertex normals.
    pub normals: Ordered<'a, Normals>,
}

/// Intermediate type for positioning properties.
#[derive(Clone, Debug)]
struct PositioningImpl {
    /// XYZ vertex positions.
    positions: Positions,

    /// XYZ vertex normals.
    normals: Maybe<[f32; 3]>,
}

/// Texturing properties.
#[derive(Clone, Debug)]
pub struct Texturing<'a> {
    /// XYZW vertex tangents, where W is the sign component.
    pub tangents: Tangents,

    /// First set of UV texture co-ordinates.
    pub tex_coords_0: Ordered<'a, TexCoordsF32>,

    /// Second set of UV texture co-ordinates.
    pub tex_coords_1: Option<Ordered<'a, TexCoordsF32>>,
}

/// Intermediate type for texturing properties.
#[derive(Clone, Debug)]
struct TexturingImpl {
    /// XYZW vertex tangents, where W is the sign component.
    pub tangents: Maybe<[f32; 4]>,

    /// First set of UV texture co-ordinates.
    pub tex_coords_0: TexCoordsF32,

    /// Second set of UV texture co-ordinates.
    pub tex_coords_1: Option<TexCoordsF32>,
}

/// Vertex coloring.
#[derive(Clone, Debug)]
pub struct Coloring<'a> {
    /// RGB(A) vertex colors.
    pub colors_0: Ordered<'a, Colors>,
}

/// Intermediate type for vertex coloring properties.
#[derive(Clone, Debug)]
struct ColoringImpl {
    /// RGB(A) vertex colors.
    pub colors_0: Colors,
}

/// Vertex skinning properties.
#[derive(Clone, Debug)]
pub struct Skinning<'a> {
    /// Indices of nodes that form the skin joints.
    pub joints_0: Ordered<'a, Joints>,

    /// Weights to be applied at each joint.
    pub weights_0: Ordered<'a, Weights>,
}

/// Intermediate type for vertex skinning properties.
#[derive(Clone, Debug)]
struct SkinningImpl {
    /// Indices of nodes that form the skin joints.
    pub joints_0: Joints,

    /// Weights to be applied at each joint.
    pub weights_0: Weights,
}

/// Geometry to be rendered with the given material.
#[derive(Clone, Debug)]
pub struct Primitive<'a> {
    /// Internal data.
    inner: PrimitiveImpl<'a>,

    /// The vertex ordering.
    order: Order<'static>,

    /// `POSITION` and `NORMAL`.
    positioning: Option<PositioningImpl>,

    /// `TANGENT`, `TEXCOORD_0`, and `TEXCOORD_1`.
    texturing: Option<TexturingImpl>,

    /// `COLOR_0`.
    coloring: Option<ColoringImpl>,

    /// `JOINTS_0` and `WEIGHTS_0`.
    skinning: Option<SkinningImpl>,
}

/// Geometry to be rendered with the given material.
#[derive(Clone, Debug)]
pub struct PrimitiveImpl<'a> {
    /// The parent `Mesh` struct.
    mesh: &'a Mesh<'a>,

    /// The corresponding JSON index.
    index: usize,

    /// The corresponding JSON struct.
    json: &'a json::mesh::Primitive,
}

impl<'a> Mesh<'a>  {
    /// Constructs a `Mesh`.
    pub fn new(gltf: &'a Gltf, index: usize, json: &'a json::mesh::Mesh) -> Self {
        Self {
            gltf: gltf,
            index: index,
            json: json,
        }
    }

    /// Returns the internal JSON index.
    pub fn index(&self) -> usize {
        self.index
    }

    /// Returns the internal JSON item.
    pub fn as_json(&self) ->  &json::mesh::Mesh {
        self.json
    }

    /// Extension specific data.
    pub fn extensions(&self) -> extensions::mesh::Mesh<'a> {
        extensions::mesh::Mesh::new(
            self.gltf,
            &self.json.extensions,
        )
    }

    /// Optional application specific data.
    pub fn extras(&self) -> &json::Extras {
        &self.json.extras
    }

    /// Optional user-defined name for this object.
    #[cfg(feature = "names")]
    pub fn name(&self) -> Option<&str> {
        self.json.name.as_ref().map(String::as_str)
    }

    /// Defines the geometry to be renderered with a material.
    pub fn primitives(&'a self) -> Primitives<'a> {
        Primitives {
            mesh: self,
            iter: self.json.primitives.iter().enumerate(),
        }
    }

    /// Defines the weights to be applied to the morph targets.
    pub fn weights(&self) -> Option<&[f32]> {
        self.json.weights.as_ref().map(Vec::as_slice)
    }
}

impl<'a> Primitive<'a> {
    /// Constructs a `Primitive`.
    ///
    /// The primitive is divided into four major properties:
    ///
    /// * Where `POSITION` is provided, the primitive will contain
    ///   `Some(Positioning)` with computed flat normals if `NORMAL`
    ///   was not provided also.
    ///
    /// * Where `TANGENT` is provided, the primitive will contain
    ///   `Some(Texturing)` along with any provided texture co-ordinates.
    ///
    /// * Where `TANGENT` is not provided but `TEXCOORD_0` and `POSITION` is,
    ///   the primitive will contain `Some(Texturing)` with computed tangents
    ///   along with any provided texture co-ordinates.
    ///
    /// * Where `COLOR_0` is provided, the primitive will contain `Some(Coloring)`
    ///
    /// * Where `JOINTS_0` and `WEIGHTS_0` are provided, the primitive will contain
    ///   `Some(Skinning)`.
    pub fn new(inner: PrimitiveImpl<'a>) -> Self {
        let tex_coords_0 = inner.find_accessor_with_semantic(Semantic::TexCoords(0));
        let colors_0 = inner.find_accessor_with_semantic(Semantic::Colors(0));
        let joints_0 = inner.find_accessor_with_semantic(Semantic::Joints(0));
        let weights_0 = inner.find_accessor_with_semantic(Semantic::Weights(0));

        let positioning = match (inner.positions(), inner.normals()) {
             (Some(positions), Some(normals)) => {
                 Some(PositioningImpl {
                     positions: positions,
                     normals: Maybe::Provided(normals.0),
                 })
            },
            (Some(positions), None) => {
                let normals: Vec<[f32; 3]> = unimplemented!(/* generate flat normals */);
                Some(PositioningImpl {
                    positions,
                    normals: Maybe::Generated(normals),
                })
            },
            _ => None,
        };

        let (order, texturing) = match (
            positioning.as_ref(),
            inner.tangents(),
            inner.tex_coords(0),
        ) {
            (_, Some(tangents), Some(tex_coords_0)) => {
                let texturing = TexturingImpl {
                    tangents: Maybe::Provided(tangents.0),
                    tex_coords_0: inner.tex_coords_f32(0).unwrap(),
                    tex_coords_1: inner.tex_coords_f32(1),
                };
                (Order::Regular, Some(texturing))
            },
            (Some(positioning), None, Some(tex_coords_0)) => {
                let (order, tangents) = unimplemented!(/* perform mikktspace */);
                let texturing = TexturingImpl {
                    tangents: Maybe::Generated(tangents),
                    tex_coords_0: inner.tex_coords_f32(0).unwrap(),
                    tex_coords_1: inner.tex_coords_f32(1),
                };
                (order, Some(texturing))
            },
            _ => (Order::Regular, None),
        };

        let coloring = colors_0.map(|_| {
            ColoringImpl {
                colors_0: inner.colors(0).unwrap(),
            }
        });

        let skinning = match (inner.joints(0), inner.weights(0)) {
            (Some(joints_0), Some(weights_0)) => {
                Some(SkinningImpl {
                    joints_0,
                    weights_0,
                })
            },
            _ => None,
        };

        Primitive {
            inner,
            order,
            positioning,
            texturing,
            coloring,
            skinning,
        }
    }

    /// Splits the primitive into its four major properties.
    pub fn split(mut self) -> (
        Option<Positioning<'a>>,
        Option<Texturing<'a>>,
        Option<Coloring<'a>>,
        Option<Skinning<'a>>,
    ) {
        unimplemented!()
    }
}

impl<'a> PrimitiveImpl<'a> {
    /// Constructs a `PrimitiveImpl`.
    pub fn new(
        mesh: &'a Mesh<'a>,
        index: usize,
        json: &'a json::mesh::Primitive,
    ) -> Self {
        Self {
            mesh: mesh,
            index: index,
            json: json,
        }
    }

    /// Returns the internal JSON item.
    pub fn as_json(&self) ->  &json::mesh::Primitive {
        self.json
    }
    
    /// Returns the vertex colors of the given set.
    pub fn colors(&self, set: u32) -> Option<Colors> {
        self.find_accessor_with_semantic(Semantic::Colors(set))
            .map(|accessor| Colors::from_accessor(accessor))
    }

    /// Returns the vertex texture co-ordinates of the given set.
    pub fn tex_coords(&self, set: u32) -> Option<TexCoords> {
        self.find_accessor_with_semantic(Semantic::TexCoords(set))
            .map(|accessor| TexCoords::from_accessor(accessor))
    }

    /// Returns the vertex texture co-ordinates of the given set, coerced into `f32`
    /// values.
    pub fn tex_coords_f32(&self, set: u32) -> Option<TexCoordsF32> {
        self.tex_coords(set).map(|iter| TexCoordsF32(iter))
    }
    
    /// Returns the joint indices of the given set.
    pub fn joints(&self, set: u32) -> Option<Joints> {
        self.find_accessor_with_semantic(Semantic::Joints(set))
            .map(|accessor| Joints::from_accessor(accessor))
    }
    
    /// Returns the joint weights of the given set.
    pub fn weights(&self, set: u32) -> Option<Weights> {
        self.find_accessor_with_semantic(Semantic::Weights(set))
            .map(|accessor| Weights::from_accessor(accessor))
    }

    /// Returns the primitive indices.
    pub fn indices(&self) -> Option<Indices> {
        self.json.indices.as_ref().map(|index| {
            let accessor = self.mesh.gltf.accessors().nth(index.value()).unwrap();
            Indices::from_accessor(accessor)
        })
    }

    /// Returns the vertex texture co-ordinates of the given set, coerced into `f32`
    /// values.
    pub fn indices_u32(&self) -> Option<IndicesU32> {
        self.indices().map(|iter| IndicesU32(iter))
    }

    /// Returns the primitive positions.
    pub fn positions(&self) -> Option<Positions> {
        self.find_accessor_with_semantic(Semantic::Positions)
            .map(|accessor| unsafe {
                Positions(accessor.iter())
            })
    }

    /// Returns the primitive normals.
    pub fn normals(&self) -> Option<Normals> {
        self.find_accessor_with_semantic(Semantic::Normals)
            .map(|accessor| unsafe {
                Normals(accessor.iter())
            })
    }

    /// Returns the primitive tangents.
    pub fn tangents(&self) -> Option<Tangents> {
        self.find_accessor_with_semantic(Semantic::Tangents)
            .map(|accessor| unsafe {
                Tangents(accessor.iter())
            })
    }

    /// Returns the attribute with the given semantic value.
    fn find_accessor_with_semantic(
        &self,
        semantic: Semantic,
    ) -> Option<accessor::Accessor<'a>> {
        self.json.attributes
            .iter()
            .find(|&(ref key, _)| key.as_ref().unwrap() == &semantic)
            .map(|(_, index)| self.mesh.gltf.accessors().nth(index.value()).unwrap())
    }

    /// Extension specific data.
    pub fn extensions(&self) -> extensions::mesh::Primitive<'a> {
        extensions::mesh::Primitive::new(
            self.mesh,
            &self.json.extensions,
        )
    }

    /// Optional application specific data.
    pub fn extras(&self) -> &json::Extras {
        &self.json.extras
    }

    /// The material to apply to this primitive when rendering
    pub fn material(&self) -> material::Material<'a> {
        self.json.material
            .as_ref()
            .map(|index| self.mesh.gltf.materials().nth(index.value()).unwrap())
            .unwrap_or_else(|| material::Material::default(self.mesh.gltf))
    }

    /// The type of primitives to render.
    pub fn mode(&self) -> Mode {
        self.json.mode.unwrap()
    }
}

impl Colors {
    fn from_accessor<'a>(accessor: Accessor<'a>) -> Colors {
        unsafe {
            match (accessor.dimensions(), accessor.data_type()) {
                (Dimensions::Vec3, DataType::U8) => {
                    Colors::RgbU8(accessor.iter())
                },
                (Dimensions::Vec4, DataType::U8) => {
                    Colors::RgbaU8(accessor.iter())
                },
                (Dimensions::Vec3, DataType::U16) => {
                    Colors::RgbU16(accessor.iter())
                },
                (Dimensions::Vec4, DataType::U16) => {
                    Colors::RgbaU16(accessor.iter())
                },
                (Dimensions::Vec3, DataType::F32) => {
                    Colors::RgbF32(accessor.iter())
                },
                (Dimensions::Vec4, DataType::F32) => {
                    Colors::RgbaF32(accessor.iter())
                },
                _ => unreachable!(),
            }
        }
    }
}

impl TexCoords {
    fn from_accessor<'a>(accessor: Accessor<'a>) -> TexCoords {
        unsafe {
            match accessor.data_type() {
                DataType::U8 => TexCoords::U8(accessor.iter()),
                DataType::U16 => TexCoords::U16(accessor.iter()),
                DataType::F32 => TexCoords::F32(accessor.iter()),
                _ => unreachable!(),
            }
        }
    }
}

impl Indices {
    fn from_accessor<'a>(accessor: Accessor) -> Indices {
        unsafe {
            match accessor.data_type() {
                DataType::U8 => Indices::U8(accessor.iter()),
                DataType::U16 => Indices::U16(accessor.iter()),
                DataType::U32 => Indices::U32(accessor.iter()),
                _ => unreachable!(),
            }
        }
    }
}

impl Joints {
    fn from_accessor<'a>(accessor: Accessor<'a>) -> Joints {
        unsafe {
            match accessor.data_type() {
                DataType::U8 => Joints::U8(accessor.iter()),
                DataType::U16 => Joints::U16(accessor.iter()),
                _ => unreachable!(),
            }
        }
    }
}

impl Weights {
    fn from_accessor<'a>(accessor: Accessor<'a>) -> Weights {
        unsafe {
            match accessor.data_type() {
                DataType::U8 => Weights::U8(accessor.iter()),
                DataType::U16 => Weights::U16(accessor.iter()),
                DataType::F32 => Weights::F32(accessor.iter()),
                _ => unreachable!(),
            }
        }
    }
}

impl Iterator for Positions {
    type Item = [f32; 3];
    fn next(&mut self) -> Option<Self::Item> {
        self.0.next()
    }
}

impl Iterator for PositionDisplacements {
    type Item = [f32; 3];
    fn next(&mut self) -> Option<Self::Item> {
        self.0.next()
    }
}

impl Iterator for Normals {
    type Item = [f32; 3];
    fn next(&mut self) -> Option<Self::Item> {
        self.0.next()
    }
}
 
impl Iterator for NormalDisplacements {
    type Item = [f32; 3];
    fn next(&mut self) -> Option<Self::Item> {
        self.0.next()
    }
}

impl Iterator for Tangents {
    type Item = [f32; 4];
    fn next(&mut self) -> Option<Self::Item> {
        self.0.next()
    }
}

impl Iterator for TangentDisplacements {
    type Item = [f32; 3];
    fn next(&mut self) -> Option<Self::Item> {
        self.0.next()
    }
}

impl<'a> Iterator for Primitives<'a> {
    type Item = Primitive<'a>;
    fn next(&mut self) -> Option<Self::Item> {
        self.iter
            .next()
            .map(|(index, json)| {
                Primitive::new(PrimitiveImpl::new(self.mesh, index, json))
            })
    }
}
