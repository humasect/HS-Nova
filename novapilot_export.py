#!BPY

"""
Name: 'Novapilot Folder (.hs)...'
Blender: 241
Group: 'Export'
Tooltip: 'Export to Novapilot folder (.hs).'
"""

__author__ = "Lyndon Tremblay (humasect)"
__url__ = ("http://localhost/")
__version__ = "0.5"
__bpydoc__ = """\

Novapilot Exporter

This script exports the model to a Novapilot folder.
"""

import Blender
import math
import os

def writeVector(f, v,):
  f.write('    Vector' + v.length + ' ')
  for s in v:
    f.write('%f, ' % s)
    i = s
  f.seek(-2, 1)
  f.write(',\n')

#def writeIntVector(file, v):
#	file.write('    (')
#	for s in v:
#		file.write('%i, ' % s)
#		i = s
#	file.seek(-2, 1)
#	file.write('),\n')

def checkMatrix(s, m):
  id = [
    [1.0, 0.0, 0.0, 0.0],
    [0.0, 1.0, 0.0, 0.0],
    [0.0, 0.0, 1.0, 0.0],
    [0.0, 0.0, 0.0, 1.0]
    ];

  wrong = 0
  for i in [0, 1, 2, 3]:
    for j in [0, 1, 2, 3]:
      if m[i][j] != id[i][j]:
        wrong = 1
        break
    if wrong == 1: break

  if wrong == 1: print 'Warning! Matrix not applied to mesh "%s" of "%s"' % (s)

def writeMatrix(f, m):
  f.write('matrix4x4 :: [[Vector4]]\n')
  f.write('matrix4x4 = [\n')
  for t in m:
    writeVector(f, t)
  f.seek(-2, 1)
  f.write(']\n\n')

#########################################

def writeBoundingSphere(f, m):
  # Does not compensate for matrix transforms.
  # (Those should be just for anim/visual)

  r = -999999
  c = [0, 0, 0]
  n = 0

  for v in m.verts:
    for i in [0, 1, 2]:
      c[i] = c[i] + v[i]
    n = n + 1

  for i in [0, 1, 2]:
    c[i] = c[i] / n

  for v in m.verts:
    for i in [0, 1, 2]:
      if v[i]-c[i] > r: r = v[i]

  f.write('sphere :: Vector4\n')
  f.write('sphere = Vector4 (%f) (%f) (%f) (%f)\n\n' % (c[0], c[1], c[2], r))

#####

def writeTriangle(f, g, a, b, c):
  f.write('\n\t\tVector3 %i %i %i,' % (g.v[a].index, g.v[b].index, g.v[c].index))
#	if mesh.hasVertexColours():
#		file.write('\t\t%i %i %i = %i %i %i %i\n'
#				   % (face.v[a].index, face.v[b].index, face.v[c].index,
#					  face.col[a].r, face.col[b].g, face.col[c].b, face.col[c].a))
#	else:
#		file.write('\t\t%i %i %i = %i %i %i %i\n'
#				   % (face.v[a].index, face.v[b].index, face.v[c].index,
#					  255, 255, 255, 255))

def writeFace(f, m, g):
  if len(g.v) >= 3:
    writeTriangle(f, g, 0, 1, 2)
    if len(g.v) == 4:
      writeTriangle(f, g, 0, 2, 3)

def writeColors(f, cs):
  for c in cs:
    f.write('\t\tColor4 %f %f %f %f,\n' %
      (float(c.r)/255.0,
       float(c.g)/255.0,
       float(c.b)/255.0,
       float(c.a)/255.0))
  f.write('\n')

def writeFaceColors(f, m, g):
  c = (g.col[0], g.col[1], g.col[2])
  if len(g.v) >= 3:
    writeColors(f, [c[0], c[1], c[2]]) #[g.col[0], g.col[1], g.col[2]])
    if len(g.v) == 4:
      writeColors(f, [c[0], c[2], g.col[3]]) #[g.col[0], g.col[2], g.col[3]])

def writeCoords(f, i, cs):
  for uv in cs:
    f.write('\t\tVector2 (%f) (%f),\n' % (uv[0], uv[1]))
  f.seek(-2, 1)
  f.write('\n')

def writeFaceCoords(f, m, g):
  if len(g.v) >= 3:
    writeCoords(f, g.image, [g.uv[0], g.uv[1], g.uv[2]])
    if len(g.v) == 4:
      writeCoords(f, g.image, [g.uv[0], g.uv[2], g.uv[3]])

def writeTex(f, g):
  if i != None: f.write('\n  Just "%s",' % g.name)
  else: f.write("\n  Nothing,")

def writeFaceTex(f, m, g):
  if len(g.v) >= 3:
    writeTex(f, g.image)
    if len(g.v) == 4: writeTex(f, g.image)

#############################

def v3subtractout(a, b, out):
  out[0] = a[0]-b[0]
  out[1] = a[1]-b[1]
  out[2] = a[2]-b[2]

def v3subtract(a, b):
  out = [a[0]-b[0], a[1]-b[1], a[2]-b[2]]
  return out

def CrossProduct(a, b, out):
  out[0] = a[1] * b[2] - a[2] * b[1]
  out[1] = a[2] * b[0] - a[0] * b[2]
  out[2] = a[0] * b[1] - a[1] * b[0]

def v3dotproduct(a, b):
  return a[0]*b[0] + a[1]*b[1] + a[2]*b[2]

def Normalize(p):
  m = math.sqrt(v3dotproduct(p, p))
  p[0] = p[0] / m
  p[1] = p[1] / m
  p[2] = p[2] / m
  # return?

def PlaneForPoints(a, b, c):
  u = v3subtract(c, a)
  v = v3subtract(c, b)

  p = [0,0,0,0]
  CrossProduct(u, v, p)
  #v3scale(plane, -1)
  Normalize(p)
  p[3] = v3dotproduct(p, a)

  return p

def writeHullFace(f, m, g):
  # TODO: catch 2-point polys

  n = 3
  if len(g.v) > 3: n = 4
  else:
    print 'Will not write 2-sided face'
    return

  f.write('\nface %i = ' % n)
  p = PlaneForPoints(m.verts[g.v[2].index],
    m.verts[g.v[1].index],
    m.verts[g.v[0].index])
  f.write('%f %f %f %f ' % (p[0], p[1], p[2], p[3]))

  for i in range(0, n): f.write('%i ' % g.v[i].index)

def exportHullST(f, m):
  f.write("-- Hull: %s" % m.name)

  f.write('hull \'%s\' %i %i [\n' % (m.name, len(m.verts), len(m.faces)))

  writeBoundingSphere(f, m)

  f.write('\tvertices [\n')
  for v in m.verts: writeVector(f, v, "Vertex")
  f.write('\t]\n')

  for fc in m.faces: writeHullFace(f, m, fc)
  f.write('\n]\n\n')


##############################

def exportMesh(f, m):
  f.write("-- Mesh: %s\n\n" % m.name)

  numTris = 0
  for g in m.faces:
    if len(g.v) == 4:
      numTris = numTris + 2
    else:
      numTris = numTris + 1

  #if mesh.hasVertexColours(): file.write('\tcolored\n')

  #writeBoundingSphere(file, mesh)
  #matrix = object.getMatrix()
  #checkMatrix(mesh.name, matrix)
  #writeMatrix(file, matrix)

  f.write('meshNamed "%s" = Mesh {\n' % m.name)

  #f.write('vertices "%s" = [' % m.name)
  f.write('\tvertices = [')
  for v in m.verts: f.write('\n\t\tVertex3 (%f) (%f) (%f),' % (v.co[0], v.co[1], v.co[2]))
  #writeVector(file, v.co)

  f.seek(-1, 1)
  f.write('\n\t],\n\n')

  #f.write('normals "%s" = [' % m.name)
  f.write('\tnormals = [')
  for v in m.verts:
    f.write('\n\t\tNormal3 (%f) (%f) (%f),' % (v.no[0], v.no[1], v.no[2]))
  f.seek(-1, 1)
  f.write('\n\t],\n\n')

  #f.write('indices "%s" = [' % m.name)
  f.write('\tindices = [')
  for fc in m.faces: writeFace(f, m, fc)
  f.seek(-1, 1)
  f.write('\n\t],\n\n')

  if m.hasFaceUV():
    f.write('\ttexcoords = Just [')
    for g in m.faces: writeFaceCoords(f, m, g)
    f.seek(-2, 1)
    f.write('\n\t],\n\n')

    f.write('\ttextures = [')
    for g in mesh.faces: writeFaceTex(f, m, g)
    f.seek(-2, 1)
    f.write('\n\t],\n\n')
  else:
    f.write('\ttexcoords = Nothing,\n\n')

  if m.hasVertexColours():
    #f.write('colors :: [Color4 Double]\n')
    f.write('\tcolors = Just [\n')
    for g in m.faces:
      writeFaceColors(f, m, g)
    f.seek(-3, 1)
    f.write('\n\t],\n\n')
  else:
    f.write('\tcolors=Nothing \n\n')

  f.seek(-3, 1)
  f.write('}\n\n')

def exportWave(f, object):
  f.write("-- Wave: %s\n\n" % object.getName())
  ipo = object.ipo

  for c in ipo.getCurves():
    points = c.getPoints()

    f.write('waveNamed "%s" = %s %s %s %i [\n' % (object.getName(), c.getName(),
      c.getInterpolation(), c.getExtrapolation(), len(points)))

    for b in points:
      pts = b.getPoints()
      if c.getName() == 'RotX' or c.getName() == 'RotY' or c.getName() == 'RotZ':
        file.write('\t(Vector2 %f %f)\n' % ((pts[0]-1)/10, pts[1]*10))
      else:
        file.write('\t(Vector2 %f %f)\n' % ((pts[0]-1)/10, pts[1]))

    file.write(']\n\n')

  file.close()

def exportObject(f, object):
  f.write("-- Object: %s\n\n" % object.getName())

  type = object.getType()

#    if object.ipo != None:
#        name = object.ipo.getName()
#        file.write("\nimport %s.Wave.%s as %s\n" % (scenename, name, name))
#    if type == 'Mesh':
#        name = object.getData().name
#        file.write("\nimport %s.Mesh.%s as %s\n" % (scenename, name, name))

  f.write('objectNamed "%s" = Vector3 (%f) (%f) (%f)\n\n'
    % (object.getName(), object.loc[0], object.loc[1], object.loc[2]))

# Add these ! and Object Keys.

#    if object.ipo != None:
#        file.write("\nipo = %s\n" % object.ipo.getName())
#    if type == 'Mesh':
#        file.write("\nmesh = %s\n" % object.getData().name)

def writeManifest(f):
  f.write("-- Manifest\n\n")
  f.write("meshes = [")
  atLeastOne = 0
  for object in Blender.Object.Get():
    if object.getType() == 'Mesh' and object.Layer == 1:
      atLeastOne = 1
      f.write('"%s", ' % object.getData().name)
  if atLeastOne == 1: f.seek(-2, 1)
  f.write("]\n\n")
  f.write("objects = [")
  for object in Blender.Object.Get():
    atLeastOne = 1
    f.write('"%s", ' % object.getName())
  if atLeastOne == 1: f.seek(-2, 1)
  f.write("]\n\n")

def exportModelST(scenename, path):
  #filename = path + '/' + scenename + '/'
  #f = open(filename + "../%s.hs" % scenename, 'w')
  f = open(path + ".hs", 'w')

  f.write("module %s where\n\n" % scenename)

  f.write("import Graphics.Rendering.OpenGL.GL.VertexSpec\n")
  f.write("import Graphics.Rendering.OpenGL.GL.CoordTrans\n\n")
  #f.write("import Resource\n")
  f.write("import Render\n\n")

  # Mesh information
  f.write("meshNamed :: String -> Mesh\n")
  #f.write("vertices :: Resource [VertexS]\n")
  #f.write("normals :: Resource [VertexS]\n")
  #f.write("texcoords :: Resource [TexCoordS]\n")
  #f.write("indices :: Resource [Vector3 Int]\n")
  #f.write("textures :: Resource [String]\n")
  # Object information
  f.write("objectNamed :: String -> Object\n\n")
  #f.write("origin :: Resource VectorS\n\n")

  writeManifest(f)

  for object in Blender.Object.Get():
    if object.getType() == 'Mesh' and object.Layer == 1:
      print '\t- Mesh: %s' % object.getData().name
      exportMesh(f, object.getData())

  for object in Blender.Object.Get():
    if object.getType() == 'Mesh' and object.Layer == 2:
      print '\t- Hull: %s' % object.data.name
      exportHullST(f, object.getData())

  for object in Blender.Object.Get():
    if object.Layer == 1 and object.ipo != None:
      print '\t- Wave: %s' % object.ipo.getName()
      exportWave(f, object)

  for object in Blender.Object.Get():
    print "\t- Object: %s" % object.getName()
    exportObject(f, object)

  f.close()

##############################

# Meshes are exported for each object they are found using. Fix that

def mkpath(p):
  if not os.path.exists(p): os.mkdir(p)

def save_nova(filename):
  filename = filename.replace("\\", "/")
  c = filename.count("/")

  path = ""
  i = 0
  m = None

  for f in filename.split("/"):
    if i == c:
      m = f.split(".")[0]
      break
    path = path + f + "/"
    i+=1

  # NOTE: Scene.getName() is not used anymore, but instead the chosen ".hs" filename
  if m == None:
    raise Exception("No manifest name: Be sure to chose an .hs filename to export")

  s = m #Blender.Scene.GetCurrent().getName()
  ps = path + s
  print "Exporting Novapilot Model to '%s'" % ps

  exportModelST(s, ps)

if __name__=='__main__':
  Blender.Window.FileSelector(save_nova, "Export Novapilot",
    Blender.sys.makename(ext='.hs'))
