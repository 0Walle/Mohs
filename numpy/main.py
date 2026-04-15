import numpy as np
from PIL import Image

def lin_reg():
    niter = 100
    alpha = 0.09
    N = 100

    X = np.random.normal(0,1,(N, 3))
    true_w = np.random.random((3))*50
    true_bias = np.random.random()
    y: np.ndarray = np.dot(X, true_w) + true_bias + np.random.rand(N)*0.01

    print(y.shape)

    w = np.ones(3)
    bias = 0

    for _ in range(niter):
        y_hat = X.dot(w) + bias
        error = y_hat-y
        grad = X.T.dot(error)/N
        grad_bias = sum(error)/N
        w -= grad * alpha
        bias -= grad_bias * alpha

    print(true_w, w)
    print(true_bias, bias)

def k_means():
    niter = 100
    k = 3
    N = 100

    def update_centroids(points: np.ndarray, labels: np.ndarray, k: int):
        N = len(labels)

        mask = np.zeros((N, k))
        mask[np.arange(N), labels] = 1

        sums = (points[:,None,:] * mask[:,:,None]).sum(0)
        counts = mask.sum(0)

        counts[counts==0] = 1

        return sums/counts[:,None]
    
    points = np.random.random((N,2))
    centroids = np.random.random((k,2))

    for i in range(niter):
        diff = (points[:,None,:] - centroids)**2
        distances = diff.sum(-1)
        labels = distances.argmin(-1)
        new_centroids = update_centroids(points, labels, k)

        if np.array_equal(centroids, new_centroids):
            break

        centroids = new_centroids

    print(centroids, i)

def PCA():
    N = 100

    X = np.random.normal(0,1,(N,2))

    Xstd = (X - X.mean(0)) / X.std(0)

    cov = np.cov(Xstd,ddof=1,rowvar=False)
    eigvalues, eigvectors = np.linalg.eig(cov)
    ord = np.argsort(eigvalues)[::-1] 
    eigvectors = eigvectors[:,ord]

    k = 2
    scores = np.matmul(Xstd, eigvectors[:,:k])

    print(scores)

def blur():
    img = Image.open('D:\Faculdade\TCC\Scala\DSL\mohs\cat.png')
    img = img.convert("RGB")

    pixels = np.array(img.get_flattened_data()).reshape((*img.size, 3))

    v = np.lib.stride_tricks.sliding_window_view(pixels, (3,3,3))

    out = np.zeros((*v.shape[:2],3),dtype=np.uint8)
    for idx in np.ndindex(v.shape[:-3]):
        blurred: np.ndarray = v[idx].reshape(9, 3).sum(0)/9
        blurred = blurred.astype(np.uint8)
        x, y, _ = idx
        out[x,y,:] = blurred[:]

    outimg = Image.fromarray(out)
    outimg.save('D:\Faculdade\TCC\Scala\DSL\\numpy\\blur.png')

    

    

blur()