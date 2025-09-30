'use client'

import { useState, useEffect } from 'react'
import { useSearchParams } from 'next/navigation'
import { useRouter } from 'next/navigation'
import {
  MagnifyingGlassIcon,
  DocumentTextIcon,
  UserIcon,
  CubeIcon,
  TruckIcon,
  CurrencyDollarIcon,
  ArrowRightIcon,
  AdjustmentsHorizontalIcon,
  XMarkIcon,
  ChevronDownIcon,
  ChevronRightIcon
} from '@heroicons/react/24/outline'
import { Card } from '@/components/UI/Card'
import PageHeader from '@/components/Layout/PageHeader'
import Input from '@/components/UI/Input'
import Button from '@/components/UI/Button'
import Select from '@/components/UI/Select'

interface SearchResult {
  id: string
  type: 'invoice' | 'customer' | 'supplier' | 'stock' | 'transaction' | 'payment'
  title: string
  description: string
  metadata: Record<string, any>
  module: string
  link: string
  score: number
  highlights: string[]
}

interface SearchFilters {
  modules: string[]
  dateFrom?: string
  dateTo?: string
  type?: string
  status?: string
  minAmount?: number
  maxAmount?: number
}

const moduleIcons = {
  sales: UserIcon,
  purchase: TruckIcon,
  stock: CubeIcon,
  gl: DocumentTextIcon,
  payments: CurrencyDollarIcon
}

const typeColors = {
  invoice: 'bg-blue-100 text-blue-700',
  customer: 'bg-green-100 text-green-700',
  supplier: 'bg-purple-100 text-purple-700',
  stock: 'bg-orange-100 text-orange-700',
  transaction: 'bg-indigo-100 text-indigo-700',
  payment: 'bg-pink-100 text-pink-700'
}

export default function AdvancedSearchPage() {
  const searchParams = useSearchParams()
  const router = useRouter()
  const initialQuery = searchParams.get('q') || ''
  
  const [query, setQuery] = useState(initialQuery)
  const [results, setResults] = useState<SearchResult[]>([])
  const [loading, setLoading] = useState(false)
  const [showFilters, setShowFilters] = useState(false)
  const [selectedResult, setSelectedResult] = useState<string | null>(null)
  
  // Search filters
  const [filters, setFilters] = useState<SearchFilters>({
    modules: ['sales', 'purchase', 'stock', 'gl', 'payments']
  })
  
  // Search statistics
  const [searchStats, setSearchStats] = useState({
    totalResults: 0,
    searchTime: 0,
    resultsByType: {} as Record<string, number>
  })

  useEffect(() => {
    if (initialQuery) {
      handleSearch()
    }
  }, [])

  const handleSearch = async () => {
    if (!query.trim()) return
    
    setLoading(true)
    const startTime = performance.now()
    
    try {
      const queryParams = new URLSearchParams({
        q: query,
        modules: filters.modules.join(','),
        ...(filters.dateFrom && { dateFrom: filters.dateFrom }),
        ...(filters.dateTo && { dateTo: filters.dateTo }),
        ...(filters.type && { type: filters.type }),
        ...(filters.status && { status: filters.status }),
        ...(filters.minAmount && { minAmount: filters.minAmount.toString() }),
        ...(filters.maxAmount && { maxAmount: filters.maxAmount.toString() })
      })
      
      const response = await fetch(`http://localhost:8000/api/v1/search?${queryParams}`)
      
      if (response.ok) {
        const data = await response.json()
        setResults(data.results || getMockSearchResults(query))
      } else {
        // Use mock data
        setResults(getMockSearchResults(query))
      }
      
      const endTime = performance.now()
      updateSearchStats(getMockSearchResults(query), endTime - startTime)
    } catch (error) {
      console.error('Search failed:', error)
      setResults(getMockSearchResults(query))
      updateSearchStats(getMockSearchResults(query), 150)
    } finally {
      setLoading(false)
    }
  }

  const getMockSearchResults = (searchQuery: string): SearchResult[] => {
    const mockResults: SearchResult[] = [
      {
        id: 'INV-2024-0156',
        type: 'invoice',
        title: 'Invoice INV-2024-0156',
        description: 'Invoice for ABC Corporation - $15,250.00',
        metadata: {
          customerName: 'ABC Corporation',
          amount: 15250.00,
          date: '2024-01-15',
          status: 'Paid'
        },
        module: 'sales',
        link: '/sales/invoices/INV-2024-0156',
        score: 0.95,
        highlights: [`Invoice number contains "${searchQuery}"`, 'Recently accessed']
      },
      {
        id: 'CUST-001',
        type: 'customer',
        title: 'ABC Corporation',
        description: 'Customer account with credit limit $50,000',
        metadata: {
          code: 'CUST-001',
          balance: 12500.00,
          creditLimit: 50000.00,
          lastOrder: '2024-01-15'
        },
        module: 'sales',
        link: '/customers/CUST-001',
        score: 0.88,
        highlights: [`Customer name matches "${searchQuery}"`, 'Active customer']
      },
      {
        id: 'STK-W001',
        type: 'stock',
        title: 'Widget A - Blue',
        description: 'Stock item with 150 units on hand',
        metadata: {
          code: 'STK-W001',
          onHand: 150,
          price: 25.99,
          category: 'Widgets'
        },
        module: 'stock',
        link: '/stock/items/STK-W001',
        score: 0.82,
        highlights: [`Item code contains "${searchQuery}"`, 'In stock']
      },
      {
        id: 'PO-2024-089',
        type: 'supplier',
        title: 'Purchase Order PO-2024-089',
        description: 'Purchase order to XYZ Supplies - $8,750.00',
        metadata: {
          supplierName: 'XYZ Supplies',
          amount: 8750.00,
          date: '2024-01-12',
          status: 'Delivered'
        },
        module: 'purchase',
        link: '/purchase/orders/PO-2024-089',
        score: 0.75,
        highlights: [`Order contains items matching "${searchQuery}"`]
      },
      {
        id: 'TRX-GL-4521',
        type: 'transaction',
        title: 'Journal Entry #4521',
        description: 'General ledger transaction - Sales revenue',
        metadata: {
          date: '2024-01-15',
          debit: 15250.00,
          credit: 15250.00,
          description: 'Sales invoice INV-2024-0156'
        },
        module: 'gl',
        link: '/gl/transactions/TRX-GL-4521',
        score: 0.70,
        highlights: [`Transaction reference matches "${searchQuery}"`]
      },
      {
        id: 'PAY-2024-234',
        type: 'payment',
        title: 'Payment Receipt #234',
        description: 'Payment from ABC Corporation - $15,250.00',
        metadata: {
          customerName: 'ABC Corporation',
          amount: 15250.00,
          date: '2024-01-20',
          method: 'Bank Transfer'
        },
        module: 'payments',
        link: '/payments/receipts/PAY-2024-234',
        score: 0.68,
        highlights: [`Payment for invoice matching "${searchQuery}"`]
      }
    ]
    
    // Filter based on actual query
    return mockResults
      .filter(result => 
        result.title.toLowerCase().includes(searchQuery.toLowerCase()) ||
        result.description.toLowerCase().includes(searchQuery.toLowerCase())
      )
      .sort((a, b) => b.score - a.score)
  }

  const updateSearchStats = (results: SearchResult[], searchTime: number) => {
    const resultsByType = results.reduce((acc, result) => {
      acc[result.type] = (acc[result.type] || 0) + 1
      return acc
    }, {} as Record<string, number>)
    
    setSearchStats({
      totalResults: results.length,
      searchTime: Math.round(searchTime),
      resultsByType
    })
  }

  const handleKeyDown = (e: React.KeyboardEvent) => {
    if (e.key === 'Enter') {
      handleSearch()
    }
  }

  const toggleModuleFilter = (module: string) => {
    setFilters(prev => ({
      ...prev,
      modules: prev.modules.includes(module)
        ? prev.modules.filter(m => m !== module)
        : [...prev.modules, module]
    }))
  }

  const clearFilters = () => {
    setFilters({
      modules: ['sales', 'purchase', 'stock', 'gl', 'payments']
    })
  }

  const getHighlightedText = (text: string, highlight: string) => {
    if (!highlight) return text
    
    const parts = text.split(new RegExp(`(${highlight})`, 'gi'))
    return parts.map((part, index) => 
      part.toLowerCase() === highlight.toLowerCase() ? 
        <span key={index} className="bg-yellow-200">{part}</span> : 
        part
    )
  }

  return (
    <div className="min-h-screen bg-gray-50">
      <PageHeader
        title="Advanced Search"
        description="Search across all modules and data"
        breadcrumbs={[
          { label: 'Dashboard', href: '/' },
          { label: 'Search' }
        ]}
      />

      <main className="max-w-7xl mx-auto px-4 sm:px-6 lg:px-8 py-8">
        {/* Search Bar */}
        <Card className="mb-6">
          <div className="p-6">
            <div className="flex items-center gap-4">
              <div className="flex-1 relative">
                <MagnifyingGlassIcon className="absolute left-3 top-3.5 h-5 w-5 text-gray-400" />
                <input
                  type="text"
                  value={query}
                  onChange={(e) => setQuery(e.target.value)}
                  onKeyDown={handleKeyDown}
                  placeholder="Search invoices, customers, stock items, transactions..."
                  className="w-full pl-10 pr-4 py-3 text-lg border border-gray-300 rounded-lg focus:ring-2 focus:ring-indigo-500 focus:border-transparent"
                  autoFocus
                />
              </div>
              <Button
                onClick={() => setShowFilters(!showFilters)}
                variant="outline"
                icon={<AdjustmentsHorizontalIcon className="h-5 w-5" />}
              >
                Filters
                {Object.keys(filters).some(k => k !== 'modules' || filters.modules.length < 5) && (
                  <span className="ml-2 h-2 w-2 bg-indigo-600 rounded-full"></span>
                )}
              </Button>
              <Button
                onClick={handleSearch}
                loading={loading}
                size="lg"
              >
                Search
              </Button>
            </div>

            {/* Filters Panel */}
            {showFilters && (
              <div className="mt-6 p-4 bg-gray-50 rounded-lg">
                <div className="grid grid-cols-1 md:grid-cols-3 gap-4">
                  {/* Module Filter */}
                  <div>
                    <label className="block text-sm font-medium text-gray-700 mb-2">
                      Modules
                    </label>
                    <div className="space-y-2">
                      {['sales', 'purchase', 'stock', 'gl', 'payments'].map(module => (
                        <label key={module} className="flex items-center">
                          <input
                            type="checkbox"
                            checked={filters.modules.includes(module)}
                            onChange={() => toggleModuleFilter(module)}
                            className="h-4 w-4 text-indigo-600 focus:ring-indigo-500 border-gray-300 rounded"
                          />
                          <span className="ml-2 text-sm text-gray-700 capitalize">{module}</span>
                        </label>
                      ))}
                    </div>
                  </div>

                  {/* Date Range */}
                  <div>
                    <label className="block text-sm font-medium text-gray-700 mb-2">
                      Date Range
                    </label>
                    <Input
                      type="date"
                      value={filters.dateFrom || ''}
                      onChange={(e) => setFilters({ ...filters, dateFrom: e.target.value })}
                      placeholder="From"
                      className="mb-2"
                    />
                    <Input
                      type="date"
                      value={filters.dateTo || ''}
                      onChange={(e) => setFilters({ ...filters, dateTo: e.target.value })}
                      placeholder="To"
                    />
                  </div>

                  {/* Amount Range */}
                  <div>
                    <label className="block text-sm font-medium text-gray-700 mb-2">
                      Amount Range
                    </label>
                    <Input
                      type="number"
                      value={filters.minAmount || ''}
                      onChange={(e) => setFilters({ ...filters, minAmount: parseFloat(e.target.value) })}
                      placeholder="Min amount"
                      className="mb-2"
                    />
                    <Input
                      type="number"
                      value={filters.maxAmount || ''}
                      onChange={(e) => setFilters({ ...filters, maxAmount: parseFloat(e.target.value) })}
                      placeholder="Max amount"
                    />
                  </div>
                </div>
                
                <div className="mt-4 flex justify-end">
                  <Button
                    variant="ghost"
                    size="sm"
                    onClick={clearFilters}
                  >
                    Clear Filters
                  </Button>
                </div>
              </div>
            )}
          </div>
        </Card>

        {/* Search Results */}
        {results.length > 0 && (
          <>
            {/* Results Summary */}
            <div className="mb-4 flex items-center justify-between">
              <div className="text-sm text-gray-600">
                Found <span className="font-semibold">{searchStats.totalResults}</span> results 
                in <span className="font-semibold">{searchStats.searchTime}ms</span>
              </div>
              <div className="flex items-center gap-4">
                {Object.entries(searchStats.resultsByType).map(([type, count]) => (
                  <span key={type} className="text-sm">
                    <span className={`inline-flex items-center px-2.5 py-0.5 rounded-full text-xs font-medium ${typeColors[type]}`}>
                      {type}: {count}
                    </span>
                  </span>
                ))}
              </div>
            </div>

            {/* Results List */}
            <div className="space-y-4">
              {results.map((result) => {
                const Icon = moduleIcons[result.module as keyof typeof moduleIcons] || DocumentTextIcon
                const isExpanded = selectedResult === result.id
                
                return (
                  <Card key={result.id} className="hover:shadow-md transition-shadow">
                    <div className="p-6">
                      <div className="flex items-start justify-between">
                        <div className="flex items-start gap-4 flex-1">
                          {/* Icon */}
                          <div className="mt-1">
                            <div className={`p-2 rounded-lg ${typeColors[result.type].replace('text-', 'bg-').replace('-700', '-100')}`}>
                              <Icon className={`h-5 w-5 ${typeColors[result.type].replace('bg-', 'text-').replace('-100', '-600')}`} />
                            </div>
                          </div>
                          
                          {/* Content */}
                          <div className="flex-1">
                            <div className="flex items-center gap-3 mb-1">
                              <h3 className="text-lg font-medium text-gray-900">
                                {getHighlightedText(result.title, query)}
                              </h3>
                              <span className={`inline-flex items-center px-2.5 py-0.5 rounded-full text-xs font-medium ${typeColors[result.type]}`}>
                                {result.type}
                              </span>
                              <span className="text-xs text-gray-500">
                                Score: {(result.score * 100).toFixed(0)}%
                              </span>
                            </div>
                            
                            <p className="text-sm text-gray-600 mb-2">
                              {getHighlightedText(result.description, query)}
                            </p>
                            
                            {/* Highlights */}
                            <div className="flex flex-wrap gap-2 mb-3">
                              {result.highlights.map((highlight, index) => (
                                <span key={index} className="text-xs text-gray-500 bg-gray-100 px-2 py-1 rounded">
                                  {highlight}
                                </span>
                              ))}
                            </div>
                            
                            {/* Expandable Metadata */}
                            <button
                              onClick={() => setSelectedResult(isExpanded ? null : result.id)}
                              className="text-sm text-indigo-600 hover:text-indigo-700 flex items-center gap-1"
                            >
                              {isExpanded ? 'Show less' : 'Show more'}
                              {isExpanded ? <ChevronDownIcon className="h-4 w-4" /> : <ChevronRightIcon className="h-4 w-4" />}
                            </button>
                            
                            {isExpanded && (
                              <div className="mt-3 pt-3 border-t border-gray-200">
                                <div className="grid grid-cols-2 gap-3 text-sm">
                                  {Object.entries(result.metadata).map(([key, value]) => (
                                    <div key={key}>
                                      <span className="text-gray-500">{key.replace(/([A-Z])/g, ' $1').trim()}:</span>
                                      <span className="ml-2 text-gray-900 font-medium">
                                        {typeof value === 'number' && key.includes('amount') 
                                          ? new Intl.NumberFormat('en-US', { style: 'currency', currency: 'USD' }).format(value)
                                          : value}
                                      </span>
                                    </div>
                                  ))}
                                </div>
                              </div>
                            )}
                          </div>
                        </div>
                        
                        {/* Action */}
                        <Button
                          size="sm"
                          variant="ghost"
                          onClick={() => router.push(result.link)}
                          icon={<ArrowRightIcon className="h-4 w-4" />}
                        >
                          View
                        </Button>
                      </div>
                    </div>
                  </Card>
                )
              })}
            </div>
          </>
        )}

        {/* Empty State */}
        {!loading && query && results.length === 0 && (
          <Card>
            <div className="p-16 text-center">
              <MagnifyingGlassIcon className="mx-auto h-12 w-12 text-gray-400" />
              <h3 className="mt-4 text-lg font-medium text-gray-900">No results found</h3>
              <p className="mt-2 text-sm text-gray-500">
                Try adjusting your search terms or filters
              </p>
            </div>
          </Card>
        )}

        {/* Initial State */}
        {!loading && !query && (
          <Card>
            <div className="p-16 text-center">
              <MagnifyingGlassIcon className="mx-auto h-12 w-12 text-gray-400" />
              <h3 className="mt-4 text-lg font-medium text-gray-900">Start searching</h3>
              <p className="mt-2 text-sm text-gray-500">
                Enter a search term to find invoices, customers, stock items, and more
              </p>
              
              {/* Popular Searches */}
              <div className="mt-8">
                <p className="text-sm text-gray-500 mb-3">Popular searches:</p>
                <div className="flex flex-wrap justify-center gap-2">
                  {['INV-2024', 'ABC Corporation', 'Widget', 'Payment', 'Stock'].map(term => (
                    <button
                      key={term}
                      onClick={() => {
                        setQuery(term)
                        setTimeout(handleSearch, 0)
                      }}
                      className="px-3 py-1 bg-gray-100 text-gray-700 rounded-full text-sm hover:bg-gray-200"
                    >
                      {term}
                    </button>
                  ))}
                </div>
              </div>
            </div>
          </Card>
        )}
      </main>
    </div>
  )
}